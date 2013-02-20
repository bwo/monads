(ns monads.writer
  (:require [monads.core :refer :all])
  (:use [monads.types :only [fst snd]]
        [monads.util :only [if-inner-return]]
        [babbage.monoid :only [<>]])
  (:import [monads.types Returned Pair]))


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

(def t writer-t)
(def m writer-m)
