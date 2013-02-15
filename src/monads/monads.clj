(ns monads.monads
  (:require [monads.core :refer :all])
  (:import [monads.core MonadOp])
  (:use [babbage.monoid :only [<>]]))

(defmacro lazy-pair [a b]
  `(lazy-seq (cons ~a (lazy-seq (cons ~b '())))))

(defmonad identity-m
  :bind (fn [m f] (f m))
  :return identity)

(def nothing nil)
(def nothing? nil?)

(deftype Just [v]
  Object
  (toString [this]
    (with-out-str (print v))))

(defn just? [v]
  (instance? Just v))

(defn from-just [^Just v]
  (cond
   (just? v) (.v v)
   (nothing? v) (throw (Exception. "Can't get something from nothing!"))
   :else (throw (Exception. (str v " is neither something nor nothing!")))))

(def just #(Just. %))

(defn maybe-t [inner]
  (let [i-return (:return inner)]
    (monad
     :return (fn [x] (i-return (just x)))
     :bind (fn [m f]
             (run-monad
              inner
              (mdo
               v <- m
               (if (nothing? v)
                 (i-return nothing)
                 (f (.v v))))))
     :monadfail {:mfail (fn [_] (i-return nothing))}
     :monadtrans {:lift (lift-m just)}
     :monadplus {:mzero (fn [_] (i-return nothing))
                 :mplus (fn [lr]
                          (let [lv (run-monad (maybe-t inner) (first lr))]
                            (or lv
                                (run-monad (maybe-t inner) (second lr)))))})))

(def maybe-m (maybe-t identity-m))

(deftype Pair [fst snd]
  Object
  (equals [this other]
    (and (instance? Pair other)
         (= fst (.fst other))
         (= snd (.snd other))))
  (hashCode [this]
    (.hashCode [fst snd]))
  (toString [this]
    (with-out-str (print [fst snd]))))

(defn fst [^Pair o] (.fst o))
(defn snd [^Pair o] (.snd o))

(declare state-t)

(defn run-state-t [m computation initial-state]
  ((run-monad m computation) initial-state))

(defn- state-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :return (curryfn [x s] (i-return (Pair. x s)))
     :bind (fn [m f]
             (fn [s]
               (run-monad
                inner
                (mdo
                 ^Pair p <- (m s)
                 let v = (fst p) s = (snd p)
                 (run-state-t (state-t inner)
                              (f v) s)))))
     :monadstate {:get-state (curryfn [_ s] (i-return (Pair. s s)))
                  :put-state (curryfn [v s] (i-return  (Pair. nil v)))}
     :monadfail (when (:monadfail inner)
                  {:mfail (fn [str] (fn [_] ((-> inner :monadfail :mfail) str)))})
     :monadplus (when (:monadplus inner)
                  (let [i-plus (-> inner :monadplus :mplus)
                        i-zero ((-> inner :monadplus :mzero) nil)]
                    {:mzero (fn [_] (fn [s] i-zero))
                     :mplus (curryfn [leftright s]
                              (i-plus
                               (lazy-pair
                                (run-state-t (state-t inner) (first leftright) s)
                                (run-state-t (state-t inner) (second leftright) s))))}))
     :monadtrans {:lift (curryfn [m s]
                          (run-monad inner (mdo
                                            v <- m
                                            (return (Pair. v s)))))})))

(def state-t (memoize state-t*))

(def state-m (state-t identity-m))

(defn run-state [computation initial-state]
  (run-state-t (state-t identity-m) computation initial-state))

(def eval-state (comp fst run-state))
(def exec-state (comp snd run-state))

;;; let's lay off the deftypes for this one
(defn right [x]
  {:val x :type ::right})
(defn left [x]
  {:val x :type ::left})

(defn either [onleft onright e]
  ((case (:type e)
     ::right onright
     ::left onleft) (:val e)))

(defn from-right [e]
  (either (fn [_] (throw (Exception. "from-right on left value!"))) identity e))

(defn from-left [e]
  (either identity (fn [_] (throw (Exception. "from-left on right value!"))) e))

(defn right? [e] (= ::right (:type e)))
(defn left? [e] (= ::left (:type e)))

(declare error-t)
(defn error-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :return (comp i-return right)
     :bind (fn [m f]
             (run-monad inner (mdo
                               x <- (run-monad (error-t inner) m)
                               (either (comp i-return left)
                                       #(run-monad (error-t inner) (f %)) x))))
     :monadtrans {:lift (fn [m] (run-monad inner (>>= m (comp i-return right))))}
     :monadfail {:mfail (comp i-return left)}
     :monadplus {:mzero (fn [_] (i-return (left nil)))
                 :mplus (fn [lr]
                          (run-monad inner (mdo l <- (run-monad (error-t inner) (first lr))
                                                (if (left? l)
                                                  (run-monad (error-t inner) (second lr))
                                                  l))))}
     :monaderror {:throw-error (comp i-return left)
                  :catch-error
                  (fn [[m handler]]
                    (run-monad
                     inner (mdo r <- (run-monad (error-t inner) m)
                                (either #(run-monad (error-t inner) (handler %))
                                        (comp i-return right)
                                        r))))})))
(def error-t (memoize error-t*))
(def error-m (error-t identity-m))

;; list-t is not always a correct transformer. Omitted.
(defmonad list-m
  :return list
  :bind (fn [m f]
          ;; inelegant: since f may return objects wrapped in Return
          ;; or singleton lists, we have to extract the results here.
          (mapcat (comp (partial run-monad list-m) f)  m))
  :monadplus {:mzero (fn [_] ())
              :mplus (fn [leftright]
                       (concat (run-monad list-m (first leftright))
                               (run-monad list-m (second leftright))))})


(defn run-reader-t [m comp e]
  ((run-monad m comp) e))

(defn run-reader [comp e]
  (run-reader-t reader-m comp e))

(declare reader-t)

(defn- reader-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :return (comp constantly i-return)
     :bind (fn [m f]
             (fn [e]
               (run-monad inner
                          (mdo
                           a <- (m e)
                           (run-reader-t (reader-t inner) (f a) e)))))
     :monadreader {:ask (fn [_] i-return)
                   :asks #(comp i-return %)
                   :local (curryfn [[f m] e]
                            (run-reader-t (reader-t inner) m (f e)))}
     :monadtrans {:lift constantly}
     :monadplus (when (:monadplus inner)
                  (let [i-zero ((-> inner :monadplus :mzero) nil)
                        i-plus (-> inner :monadplus :mplus)]
                    {:mzero (fn [_] (constantly i-zero))
                     :mplus (curryfn [leftright e] 
                              (i-plus (lazy-pair
                                       (run-reader-t (reader-t inner) (first leftright) e)
                                       (run-reader-t (reader-t inner) (second leftright) e))))})))))

(def reader-t (memoize reader-t*))
(def reader-m (reader-t identity-m))

(deftype Cont [c v]
    Object
  (toString [this]
    (with-out-str (print [c v]))))

(defn- cont? [o]
  (instance? Cont o))

(defmonad cont-m
  :return (curryfn [r c] (Cont. c r))
  :bind (fn [m f]
          (fn [r]
            (Cont. m (fn [v] (Cont. (f v) r)))))
  :monadcont {:callcc (curryfn [f c]
                        (Cont. (f (curryfn [v _] (Cont. c v))) c))})

;; this is faster, but blows the stack.
(defmonad cont-m2
  :return (curryfn [r c] (c r))
  :bind (fn [m f]
          (fn [r]
            ((run-monad cont-m2
                        (m f)) r))))

(defn cont-t [inner]
  (let [i-return (:return inner)]
    (assoc cont-m
      :monadtrans {:lift (curryfn [m c] (c (run-monad inner m)))})))

(defn run-cont [m & c]
  (let [m ((run-monad cont-m m) c)]
    (if (cont? m)
      (recur (.c m) (.v m))
      m)))

(defn run-cont-t [m comp cont]
  (let [comp ((run-monad m comp) cont)]
    (if (cont? comp)
      (recur m (.c comp) (.v comp))
      comp)))

(defmonad writer-m
  :return (fn [v] (Pair. v nil))
  :bind (fn [m f]
          (let [^Pair m (run-monad writer-m m)
                ^Pair m' (run-monad writer-m (f (fst m)))]
            (Pair. (fst m') (<> (snd m) (snd m')))))
  :monadwriter {:tell (partial ->Pair nil)
                :listen (fn [m]
                          (let [p (run-monad writer-m m)]
                            (Pair. p (snd p))))})

(defn listens [f m]
  (mdo p <- (listen m)
       (return (fst p) (f (snd p)))))
(defn censor [f m]
  (pass (mdo a <- m
             (return (a, f)))))

;; tree-numbering.
;; Our trees: {:val int :left tree :right tree}, or nil
(defn node [v left right]
  {:val v :left left :right right})
(defn index-in-list [p lst]
  (second (first (filter (comp p first) (map vector lst (range))))))
(defn n-node [x table]
  (if-let [i (index-in-list (partial = x) table)]
    [table i]
    [(conj table x) (count table)]))
(defn number-node [x]
  (mdo table <- get-state
       let [newtable newpos] = (n-node x table)
       (put-state newtable)
       (return newpos)))
(defn number-tree [{:keys [val left right] :as tree}]
  (if-not tree
    (return nil)
    (mdo num <- (number-node val)
         nt1 <- (number-tree left)
         nt2 <- (number-tree right)
         (return (node num nt1 nt2)))))
(defn num-tree [t]
  (eval-state (number-tree t) []))

