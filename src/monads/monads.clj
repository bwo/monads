(ns monads.monads
  (:require [monads.core :refer :all])
  (:import [monads.core MonadOp]))

(defmacro lazy-pair [a b]
  `(lazy-seq (cons ~a (lazy-seq (cons ~b '())))))

(def nothing ::nothing)

(defn nothing? [v]
  (= v nothing))

(deftype Just [v]
  Object
  (toString [this]
    (with-out-str (print v))))

(defn just? [v]
  (instance? Just v))

(defn from-just [v]
  (cond
   (just? v) (.v v)
   (nothing? v) (throw (Exception. "Can't get something from nothing!"))
   :else (throw (Exception. (str v " is neither something nor nothing!")))))

(def just #(Just. %))

(defmonad maybe-m
  :return just
  :bind (fn [m f]
          (if (nothing? m)
            m
            (f (.v m))))
  :monadplus {:mzero (fn [_] nothing)
              :mplus (fn [leftright]
                       (let [lv (run-monad maybe-m (first leftright))]
                         (if (just? lv)
                           lv
                           (run-monad maybe-m (second leftright)))))}
  :monadfail {:mfail (fn [_] nothing)})

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
     :monadtrans {:lift (lift-m just)})))

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

(declare state-m)

(defn run-state [comp initial-state]
  ((run-monad state-m comp) initial-state))

(defn state-return [x]
  (fn inner-state-return [s] (Pair. x s)))

(defn state-bind [m f]
  (fn inner-state-bind [s]
    (let [p (m s)
          v (.fst p)
          s' (.snd p)]
      (run-state (f v) s'))))

(defmonad state-m
  :return state-return
  :bind state-bind
  :monadstate {:get-state (curryfn [_ s] (Pair. s s))
               :put-state (curryfn [v _] (Pair. nil v))})
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
                 p <- (m s)
                 let v = (.fst p) s = (.snd p)
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

;;; let's lay off the deftypes for this one
(defn right [x]
  {:val x :type ::right})
(defn left [x]
  {:val x :type ::left})

(defmonad either-m
  :bind (fn [m f]
          (if (= ::left (:type m))
            m
            (f (:val m))))
  :return right
  :monadfail {:mfail left})

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

(defmonad identity-m
  :bind (fn [m f] (f m))
  :return identity)

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

(declare run-reader)

(defmonad reader-m
  :return constantly
  :bind (fn [m f] ;; (e -> a) -> (a -> (e -> b)) -> (e -> b) / the "s" combinator
          (fn [e]
            (let [a (m e)]
              (run-reader (f a) e))))
  :monadreader {:ask (fn [_] identity)
                :asks identity
                :local (fn [[f m]]
                         (fn [e]
                           (run-reader m (f e))))})

(defn run-reader-t [m comp e]
  ((run-monad m comp) e))

(defn run-reader [comp e]
  ((run-monad reader-m comp) e))

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
