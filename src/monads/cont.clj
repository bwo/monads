(ns monads.cont
  (:require [monads.core :refer :all :exclude [reorganize]]
            [monads.types :as types :refer [if-instance]])
  (:use [monads.util :only [curryfn]])
  (:import [monads.types Returned Mplus Bind]))

;; this type shouldn't be exposed
(deftype Cont [c v]
    Object
  (toString [this]
    (with-out-str (print [c v]))))

(defmonad cont-m
  (mreturn [me r] (fn [c] (Cont. c r)))
  (bind [me m f] (fn [r] (Cont. m (fn [v] (Cont. (f v) r))))))

;; note: no use of m!
(defn callcc [f]
  (Returned. (curryfn [m c] (Cont. (f (curryfn [v _] (Cont. c v))) c))))

(defn cont-t [inner]
  (monad
   (mreturn [me r] (fn [c] (Cont. c r)))
   (bind [me m f] (fn [r] (Cont. m (fn [v] (Cont. (f v) r)))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (fn [c] (run-monad inner (>>= m c))))))

(defn run-cont [m]
  (loop [m m c identity]
    (let [m ((run-monad cont-m m) c)]
      (if-instance Cont m
        (recur (.c m) (.v m))
        m))))

(defn run-c [c]
  (loop [c c f identity]
    (if-instance Cont c
      (recur (.c c) (.v c))
      (f c))))

;; after http://okmij.org/ftp/continuations/ContTutorial.hs, loosely.

(defn reset [c]
  (return (run-cont c)))

(defn shift [f]
  (Returned.
   (fn [m]
     (fn [c]
       (run-cont (f #(run-cont (run-c (c %)))))))))

(defn run-cont-t [m comp cont]
  (let [comp ((run-monad m comp) cont)]
    (if-instance Cont comp
      (recur m (.c comp) (.v comp))
      comp)))

(def t cont-t)
(def m cont-m)

(declare reorg-binds)

(defn- reorg-plus [m]
  (if-instance Mplus m
    (let [l (.l m)
          r (.r m)]
      (mdo li <- (reorg-binds l)
           ri <- (reorg-binds r)
           ri <- (if-instance Mplus ri
                   (let [lr (.l ri)]
                     (if-instance Mplus lr
                       (reorg-plus ri)
                       (return ri)))
                   (return ri))
           (if-instance Mplus li
             (let [l-l (.l li)]
               (mdo ri <- (reorg-plus ri)
                    (reorg-plus (Mplus. l-l (Mplus. (.r li) ri)))))
             (return (Mplus. li ri)))))
    (return m)))

(defn- reorg-binds [m]
  (if-instance Bind m
    (let [comp (.comp m)]
      (if-instance Bind comp
        (let [i-comp (.comp comp)
              i-f (.f comp)
              f (.f m)]
          (mdo i <- (reorg-plus i-comp)
               (reorg-binds (Bind. i (fn [x] (Bind. (i-f x) f))))))
        (mdo comp <- (reorg-plus comp)
             (return (Bind. comp (.f m))))))
    (return m)))

(defn reorganize
  "Reorganize the monadic computation m so that binds nested on the
   left are moved to the right, i.e. transform expressions like

   (>>= (>>= (>>= m f) g) h)

   into expressions like

   (>>= m (fn [x] (>>= (f x) (fn [y] (>>= (g y) h))))).

   A monad implementation for which these two expressions give
   different results is broken.

   Similarly reorganizes mplus operations nested on the left:

   (mplus (mplus (mplus a b) c) d)

   Becomes

   (mplus a (mplus b (mplus c d))).

   And, similarly, an mplus operation which is not associative is
   considered to be in error.

   Both transformations are interleaved:

   (mplus (mplus (>>= (>>= (mplus (mplus a b) c) f) g) d) e)

   becomes

   (mplus (>>= (mplus a (mplus b c)) (fn [x] (>>= (f x) g))) (mplus d e)).

   Note that *only* mplus and bind operations are investigated: the
   reorganization does not recurse into the monadic-computation
   arguments of e.g. listen, local, or pass."
  [m]
  ;; this reorg-plus is here in case we fall through reorg-binds right
  ;; away. We can't call reorg-plus in reorg-binds in the else branch
  ;; of the first if-instance, because it can lead to, yes, stack
  ;; overflows.
  (run-cont (reorg-plus (run-cont (reorg-binds m)))))
