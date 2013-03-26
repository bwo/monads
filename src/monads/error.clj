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
                                     (return l))))}
     :monaderror {:throw-error (comp i-return left)
                  :catch-error (fn [comp handler]
                                 (run-mdo inner
                                          v <- (run-monad (error-t inner) comp)
                                          (either #(run-monad (error-t inner) (handler %))
                                                  (comp i-return right) v)))})))

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
                             v)))}
    :monaderror {:throw-error left
                 :catch-error (fn [comp handler]
                                (let [v (run-monad error-m comp)]
                                  (either #(run-monad error-m (handler %)) right v)))}))

(def error-t (memoize error-t*))

(def m error-m)
(def t error-t)
