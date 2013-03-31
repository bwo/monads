(ns monads.writer
  (:require [monads.core :refer :all]
            [babbage.monoid :as m])
  (:use [monads.types :only [fst snd]]
        [monads.util :only [lazy-pair]]
        [babbage.monoid :only [<>]])
  (:import [monads.types Returned Pair]))

(extend-protocol m/Monoid
  clojure.lang.PersistentVector$ChunkedSeq
  (mempty? [self] (empty? self))
  (mempty [self] [])
  (value [self] self)
  (<> [self o] (concat self o)))

(declare writer-t)

(defn writer-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (fn [v] (i-return (Pair. v nil)))
     :monadfail (when (:monadfail inner)
                  (let [ifail (-> inner :monadfail :mfail)]
                    {:mfail (fn [msg]
                              (ifail msg))}))
     :monadtrans {:lift (fn [m]
                          (run-mdo inner
                                   a <- m
                                   (return (Pair. a nil))))}
     :monadplus (when (:monadplus inner)
                  (let [i-plus (-> inner :monadplus :mplus)
                        i-zero (-> inner :monadplus :mzero)]
                    {:mzero i-zero
                     :mplus (fn [lr]
                              (i-plus (lazy-pair
                                       (run-monad (writer-t inner) (first lr))
                                       (run-monad (writer-t inner) (second lr)))))}))
     :monadwriter {:tell (fn [w] (i-return (Pair. nil w)))
                   :listen (fn [comp]
                             (run-mdo inner
                                      ^Pair p <- (run-monad (writer-t inner) comp)
                                      (return (Pair. [(fst p) (snd p)] (snd p)))))
                   :pass (fn [comp]
                           (run-mdo inner
                                    ^Pair p <- (run-monad (writer-t inner) comp)
                                    (return (Pair. (first (fst p))
                                                   ((second (fst p)) (snd p))))))}
     :bind (fn [m f]
             (run-mdo inner
                      ^Pair p <- (run-monad (writer-t inner) m)
                      let a = (fst p) w = (snd p)
                      ^Pair p <- (run-monad (writer-t inner) (f a))
                      let b = (fst p) w' = (snd p)
                      (return (Pair. b (<> w w'))))))))

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
            (Pair. b (<> w w'))))
  :monadwriter {:tell (fn [w] (Pair. nil w))
                :listen (fn [comp]
                          (let [^Pair p (run-monad writer-m comp)]
                            (Pair. [(fst p) (snd p)] (snd p))))
                :pass (fn [comp]
                        (let [^Pair p (run-monad writer-m comp)]
                          (Pair. (first (fst p))
                                 ((second (fst p)) (snd p)))))})


(def t writer-t)
(def m writer-m)

(defn lift-catch [m h]
  (Returned.
   (fn [t]
     (run-monad (:inner t)
                (catch-error (run-monad t m)
                             (fn [err] (run-monad t (h err))))))))
