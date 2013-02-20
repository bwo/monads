(ns monads.error
  (:require [monads.core :refer :all])
  (:use [monads.util :only [if-inner-return]]
        [monads.types :only [from-right from-left right left left? either]])
  (:import [monads.types Returned Either]))


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

(def m error-m)
(def t error-t)
