(ns monads.reader
  (:require [monads.core :refer :all])
  (:use [monads.util :only [curryfn lazy-pair if-inner-return]])
  (:import [monads.types Returned]))

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
                          (run-reader comp (f e))))))

(defn run-reader [comp e]
  ((run-monad reader-m comp) e))

(def t reader-t)
(def m reader-m)
