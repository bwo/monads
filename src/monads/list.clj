(ns monads.list
  (:require [monads.core :refer :all]
            [monads.util :as u]))


(defn mcat [f xs]
  (lazy-seq
   (if (not (seq xs))
     nil
     (concat (f (first xs)) (mcat f (rest xs))))))

(defmonad list-m
  :return list
  :bind (fn [m f]
          (mcat (comp (partial run-monad list-m) f)  m))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (concat (run-monad list-m (first leftright))
                               (run-monad list-m (second leftright))))})

;; note that this is not always a monad.
(declare list-t)
(defn list-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :return (comp i-return list)
     :bind (fn [m f]
             (run-mdo inner
                      a <- (run-monad (list-t inner) m)
                      b <- (u/sequence-m (map (comp (partial run-monad (list-t inner)) f) a))
                      (return (apply concat b))))
     :monadplus {:mzero (i-return ())
                 :mplus (fn [lr]
                          (run-mdo inner
                                   a <- (run-monad (list-t inner) (first lr))
                                   b <- (run-monad (list-t inner) (second lr))
                                   (return (concat a b))))}
     :monadtrans {:lift (fn [m]
                          (run-mdo inner
                                   a <- (run-monad (list-t inner) m)
                                   (return (list a))))})))
(def list-t (memoize list-t*))

(def m list-m)
(def t list-t)
