(ns monads.examples.evaluator2
  (:require [monads.core :refer :all]
            [monads.state :as s]
            [monads.writer :as w])
  (:use [monads.util :only [lift-m lift-m*]]))

(defn const [x]
  (if (symbol? x)
    (lift-m #(get % x) s/get-state)
    (return x)))

(declare run)

(defn calc [op x y msg]
  (mdo
   (lift (w/tell [(str msg ": " x ", " y)]))
   (lift-m* op (run x) (run y))))

(defn decl [x y]
  (mdo v <- (run y)
       (s/modify #(assoc % x v))
       (lift (w/tell [(str "decl " x)]))
       (return v)))

(defn run [op]
  (if (list? op)
    (case (second op)
      + (calc + (first op) (last op) "plus")
      - (calc - (first op) (last op) "minus")
      * (calc * (first op) (last op) "times")
      / (calc / (first op) (last op) "div")
      = (decl (first op) (last op)))
    (const op)))

(def table {'pi 3.141592 'e 2.718281 'deg 57.295779})

(defn evaluate [op]
  (s/run-state-t (s/t w/m) (run op) table))
