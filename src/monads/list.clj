(ns monads.list
  (:require [monads.core :refer :all]))


(defn flatten-1
  [seqs]
  (lazy-seq
   (when-let [s (seq seqs)]
     (concat (first s) (flatten-1 (rest s))))))

;; list-t is not always a correct transformer. Omitted.
(defmonad list-m
  :return list
  :bind (fn [m f]
          ;; inelegant: since f may return objects wrapped in Return
          ;; or singleton lists, we have to extract the results here.
          (flatten-1 (map (comp (partial run-monad list-m) f)  m)))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (concat (run-monad list-m (first leftright))
                               (run-monad list-m (second leftright))))})

(def m list-m)
