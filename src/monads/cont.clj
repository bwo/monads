(ns monads.cont
  (:require [monads.core :refer :all]
            [monads.types :as types :refer [if-instance cond-instance]])
  (:use [monads.util :only [curryfn]])
  (:import [monads.types Returned Mplus Bind]))

(defn callcc [f]
  (Returned.
   (curryfn [m c]
     ((run-monad m (f (fn [a] (fn [_] (c a))))) c))))

(set! *warn-on-reflection* true)

(deftype C [v c]
  Object
  (toString [this]
    (with-out-str (print [v c]))))
(deftype Done [v]
  Object
  (toString [this]
    (with-out-str (print v))))

(defn tramp [cur]
  (let [stack '()]
    (loop [cur cur stack stack]
      (cond-instance cur
        Done (let [[hd & tl] stack]
               (if (nil? hd)
                 (.v cur)
                 (recur (hd (.v cur)) tl)))
        C    (recur (.v cur) (cons (.c cur) stack))))))

(defn run-cont-t [m computation]
  (tramp ((run-monad m computation) (comp ->Done return))))

(defn cont-t [inner]
  (monad
   (mreturn [me r] (fn [k] (C. (Done. r) k)))
   (bind [me m f] (fn [k]
           (m (fn [a]
                (C. (Done. (f a))
                    (fn [cmp]
                      (C. (Done. (run-monad me cmp))
                          (fn [z]
                            (z k)))))))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (fn [c] 
                  (Done. (>>= m (comp tramp c)))))))

(def cont-m
  (monad
   (mreturn [me r] (fn [k] (C. (Done. r) k)))
   (bind [me m f] (fn [k]
                    (m (fn [a]
                         (C. (Done. (f a))
                             (fn [cmp]
                               (C. (Done. (run-monad me cmp))
                                   (fn [z]
                                     (z k)))))))))))

(defn run-cont [m]
  (tramp ((run-monad cont-m m) ->Done)))

;; after http://okmij.org/ftp/continuations/ContExample.hs

(defn shift [f]
  (Returned.
   (fn [m]
     (fn [c]
       (let [r (f (comp tramp c))]
         (Done. (if (types/monadtrans? m)
                  (run-cont-t m r)
                  (run-cont r))))))))

(defn reset [computation]
  (Returned.
   (fn [m]
     (run-monad m (if (types/monadtrans? m)
                    (lift (run-cont-t m computation))
                    (return (run-cont computation)))))))

(def t cont-t)
(def m cont-m)

(declare reorg-binds)

(defn reorg-plus [m]
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

(defn reorg-binds [m]
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
