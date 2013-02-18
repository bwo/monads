(ns monads.monads
  (:require [monads.core :refer :all])
  (:import [monads.core Returned])
  (:use [babbage.monoid :only [<>]]))

(set! *warn-on-reflection* true)

(defmacro lazy-pair [a b]
  `(lazy-seq (cons ~a (lazy-seq (cons ~b '())))))

(defmacro if-inner-return [m ifb elseb]
  `(if-let [~'i-return (-> ~m :inner :return)]
     ~ifb
     ~elseb))

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

(defn maybe [on-nothing on-just m]
  (if m
    (on-just (from-just m))
    on-nothing))

(defn maybe-t [inner]
  (let [i-return (:return inner)]
    (monad
     :return (fn [x] (i-return (just x)))
     :bind (fn [m f] (run-mdo inner
                             v <- m
                             (if (nothing? v)
                               (i-return nothing)
                               (run-monad (maybe-t inner) (f (from-just v))))))
     :monadfail {:mfail (fn [_] (i-return nothing))}
     :monadtrans {:lift (partial lift-m just)}
     :monadplus {:mzero (i-return nothing)
                 :mplus (fn [lr]
                          (let [lv (run-monad (maybe-t inner) (first lr))]
                            (or lv
                                (run-monad (maybe-t inner) (second lr)))))})))

(defmonad maybe-m
  :return just
  :bind (fn [m f]
          (let [v (run-monad maybe-m m)]
            (when v (run-monad maybe-m (f (from-just v))))))
  :monadfail {:mfail (constantly nothing)}
  :monadplus {:mzero nothing
              :mplus (fn [lr]
                       (let [lv (run-monad maybe-m (first lr))]
                         (or lv
                             (run-monad maybe-m (second lr)))))})

(deftype Pair [fst snd]
  clojure.lang.Seqable
  (seq [_] (list fst snd))
  Object
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
     :inner inner
     :return (curryfn [x s] (i-return (Pair. x s)))
     :bind (fn [m f]
             (fn [s]
               (run-mdo inner
                        ^Pair p <- (m s)
                        let v = (fst p) s = (snd p)
                        (run-state-t (state-t inner)
                                     (f v) s))))
     :monadfail (when (:monadfail inner)
                  {:mfail (curryfn [str _] ((-> inner :monadfail :mfail) str))})
     :monadplus (when (:monadplus inner)
                  (let [i-plus (-> inner :monadplus :mplus)
                        i-zero ((-> inner :monadplus :mzero) nil)]
                    {:mzero (fn [_] i-zero)
                     :mplus (curryfn [leftright s]
                              (i-plus
                               (lazy-pair
                                (run-state-t (state-t inner) (first leftright) s)
                                (run-state-t (state-t inner) (second leftright) s))))}))
     :monadtrans {:lift (curryfn [m s]
                          (run-mdo inner
                                   v <- m
                                   (return (Pair. v s))))})))
(def state-t (memoize state-t*))

(defn state-return [x]
  (fn inner-state-return [s] (Pair. x s)))

(declare run-state)

(defmonad state-m
  :return (curryfn [x s] (Pair. x s))
  :bind (fn [m f]
          (fn [s]
            (let [^Pair p (m s)]
              (run-state (f (fst p)) (snd p))))))

(defn run-state [computation initial-state]
  ((run-monad state-m computation) initial-state))

(def get-state (Returned. (curryfn [m s]
                            (if-inner-return m
                              (i-return (Pair. s s))
                              (Pair. s s)))))

(def get-state (Returned. (curryfn [m s]
                            (if-let [i-return (-> m :inner :return)]
                              (i-return (Pair. s s))
                              (Pair. s s)))))
(defn put-state [v] (Returned. (curryfn [m s]
                                 (if-let [i-return (-> m :inner :return)]
                                   (i-return (Pair. nil v))
                                   (Pair. nil v)))))
(defn modify [f] (>>= get-state (comp put-state f)))

(def eval-state (comp fst run-state))
(def exec-state (comp snd run-state))
(defn exec-state-t [m comp initial-state]
  (run-monad (:inner m) (lift-m snd (run-state-t m comp initial-state))))
(defn eval-state-t [m comp initial-state]
  (run-monad (:inner m) (lift-m fst (run-state-t m comp initial-state))))

(deftype Either [v type]
  Object
  (toString [this]
    (with-out-str (print [type v]))))

(defn right? [^Either o]
  (= :right (.type o)))
(defn left? [^Either o]
  (= :left (.type o)))

;;; let's lay off the deftypes for this one
(defn right [x]
  (Either. x :right))
(defn left [x]
  (Either. x :left))

(defn either [onleft onright ^Either e]
  ((case (.type e)
     :right onright
     :left onleft) (.v e)))

(defn from-right [^Either e]
  (either (fn [_] (throw (Exception. "from-right on left value!"))) identity e))

(defn from-left [^Either e]
  (either identity (fn [_] (throw (Exception. "from-left on right value!"))) e))

(declare error-t)
(defn error-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (comp i-return right)
     :bind (fn [m f]
             (run-mdo inner 
                      x <- (run-monad (error-t inner) m)
                      (either (comp i-return left)
                              #(run-monad (error-t inner) (f %)) x)))
     :monadtrans {:lift (fn [m] (run-monad inner (>>= m (comp i-return right))))}
     :monadfail {:mfail (comp i-return left)}
     :monadplus {:mzero (i-return (left nil))
                 :mplus (fn [lr]
                          (run-mdo inner
                                   l <- (run-monad (error-t inner) (first lr))
                                   (if (left? l)
                                     (run-monad (error-t inner) (second lr))
                                     l)))})))

(let [mzero (left nil)]
  (defmonad error-m
    :return right
    :bind (fn [m f]
            (let [r (run-monad error-m m)]
              (either left #(run-monad error-m (f %)) r)))
    :monadfail {:mfail left}
    :monadplus {:mzero mzero
                :mplus (fn [lr]
                         (let [v (run-monad error-m (first lr))]
                           (if (left? v)
                             (run-monad error-m (second lr))
                             v)))}))

(defn throw-error [e] (Returned. (fn [m]
                                   (if-inner-return m
                                     (i-return (left e))
                                     (left e)))))
(defn catch-error [comp handler]
  (Returned. (fn [m]
               (if-inner-return m
                 (run-mdo (:inner m)
                          v <- (run-monad m comp)
                          (either #(run-monad m (handler %))
                                  (comp i-return right) v))
                 (let [v (run-monad m comp)]
                   (either #(run-monad m (handler %)) right v))))))

(def error-t (memoize error-t*))

(defn flatten-1
  [seqs]
  (lazy-seq
   (when-let [s (seq seqs)]
     (concat (first s) (flatten-1 (rest s))))))

;; list-t is not always a correct transformer. Omitted.
(defmonad list-m
  :return list
  :bind (fn [m f]
          ;; inelegant: since f may return objects wrapped in Return
          ;; or singleton lists, we have to extract the results here.
          (flatten-1 (map (comp (partial run-monad list-m) f)  m)))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (concat (run-monad list-m (first leftright))
                               (run-monad list-m (second leftright))))})


(defn run-reader-t [m comp e]
  ((run-monad m comp) e))

(declare reader-t)

(defn- reader-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (comp constantly i-return)
     :bind (fn [m f]
             (fn [e]
               (run-mdo inner
                        a <- (m e)
                        (run-reader-t (reader-t inner) (f a) e))))
     :monadtrans {:lift constantly}
     :monadplus (when (:monadplus inner)
                  (let [i-zero (-> inner :monadplus :mzero)
                        i-plus (-> inner :monadplus :mplus)]
                    {:mzero (fn [_] (constantly i-zero))
                     :mplus (curryfn [leftright e]
                              (i-plus (lazy-pair
                                       (run-reader-t (reader-t inner) (first leftright) e)
                                       (run-reader-t (reader-t inner) (second leftright) e))))})))))

(def reader-t (memoize reader-t*))

(declare run-reader)

(defmonad reader-m
  :return constantly
  :bind (fn [m f]
          (fn [e]
            (run-reader (f (run-reader m e)) e))))

(def ask (Returned. (fn [m]
                      (if-inner-return m
                        i-return
                        identity))))
;; or: (defn asks [f] (lift-m f ask))
(defn asks [f] (Returned. (fn [m]
                           (if-inner-return m
                             (comp i-return f)
                             f))))
(defn local [f comp] (Returned.
                      (curryfn [m e]
                        (if-inner-return m
                          (run-reader-t m comp (f e))
                          (run-reader m comp (f e))))))

(defn run-reader [comp e]
  ((run-monad reader-m comp) e))

(deftype Cont [c v]
    Object
  (toString [this]
    (with-out-str (print [c v]))))

(defn get-cont [^Cont c]
  (.c c))
(defn get-arg [^Cont c]
  (.v c))

(defn- cont? [o]
  (instance? Cont o))

(defmonad cont-m
  :return (curryfn [r c] (Cont. c r))
  :bind (fn [m f]
          (fn [r]
            (Cont. m (fn [v] (Cont. (f v) r))))))

;; note: no use of m!
(defn callcc [f]
  (Returned. (curryfn [m c] (Cont. (f (curryfn [v _] (Cont. c v))) c))))

(defn cont-t [inner]
  (let [i-return (:return inner)]
    (assoc cont-m
      :monadtrans {:lift (curryfn [m c] (run-monad inner (>>= m c)))}
      :inner inner)))

(defn run-cont [m c]
  (let [m ((run-monad cont-m m) c)]
    (if (cont? m)
      (recur (get-cont m) (get-arg m))
      m)))

(defn run-cont-t [m comp cont]
  (let [comp ((run-monad m comp) cont)]
    (if (cont? comp)
      (recur m (get-cont comp) (get-arg comp))
      comp)))

(declare writer-t)
(defn writer-t* [inner]
  (let [i-return (:return inner)]
    :inner inner
    :return (fn [v] (i-return (Pair. v nil)))
    :bind (fn [m f]
            (run-mdo inner
                     ^Pair p <- (run-monad (writer-t inner) m)
                     let a = (fst p) w = (snd p)
                     ^Pair p <- (run-monad (writer-t inner) (f a))
                     let b = (fst p) w' = (snd p)
                     (return (Pair. b (<> w w')))))))

(def writer-t (memoize writer-t*))

(defmonad writer-m
  :return (fn [v] (Pair. v nil))
  :bind (fn [m f]
          (let [^Pair p (run-monad writer-m m)
                a (fst p)
                w (snd p)
                ^Pair p (run-monad writer-m (f a))
                b (fst p)
                w' (snd p)]
            (Pair. b (<> w w')))))

(defn tell [w] (Returned. (fn [m]
                            (if-inner-return m
                              (i-return (Pair. nil w))
                              (Pair. nil w)))))

(defn listen [comp] (Returned.
                     (fn [m]
                       (if-inner-return m
                        (run-mdo (:inner m)
                                 ^Pair p <- (run-monad m comp)
                                 (return (Pair. [(fst p) (snd p)] (snd p))))
                        (let [^Pair p (run-monad m comp)]
                          (Pair. [(fst p) (snd p)] (snd p)))))))

(defn pass [comp] (Returned.
                   (fn [m]
                     (if-inner-return m
                       (run-mdo (:inner m)
                                ^Pair p <- (run-monad m comp)
                                (return (Pair. (first (fst p))
                                               ((second (fst p)) (snd p)))))
                       (let [^Pair p (run-monad m comp)]
                         (Pair. (first (fst p))
                                ((second (fst p)) (snd p))))))))

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
  (if-let [i (get table x)]
    [table i]
    (let [c (count table)]
      [(assoc table x c) c])))
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
  (eval-state (number-tree t) {}))

(defn tree [n]
  (reduce #(node %2 %1 %1) nil (range n)))
