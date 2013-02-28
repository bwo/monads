(ns monads.list
  (:require [monads.core :refer :all]))


(defn foldcat [f acc xs]
  (lazy-seq
   (if (not (seq xs))
     acc    
     (f (first xs) (foldr f acc (rest xs))))))

;; list-t is not always a correct transformer. Omitted.
(defmonad list-m
  :return list
  :bind (fn [m f]
          ;; inelegant: since f may return objects wrapped in Return
          ;; or singleton lists, we have to extract the results here.
          (foldcat (fn [x acc]
                   (concat (run-monad list-m (f x)) acc))  '()  m))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (concat (run-monad list-m (first leftright))
                               (run-monad list-m (second leftright))))})

(def m list-m)
