(ns monads.cont
  (:require [monads.core :refer :all])
  (:use [monads.util :only [curryfn]])
  (:import [monads.types Returned]))

;; this type shouldn't be exposed
(deftype Cont [c v]
    Object
  (toString [this]
    (with-out-str (print [c v]))))

(defn get-cont [^Cont c]
  (.c c))
(defn get-arg [^Cont c]
  (.v c))

(defn- cont? [o]
  (instance? Cont o))

(defmonad cont-m
  :return (curryfn [r c] (Cont. c r))
  :bind (fn [m f]
          (fn [r]
            (Cont. m (fn [v] (Cont. (f v) r))))))

;; note: no use of m!
(defn callcc [f]
  (Returned. (curryfn [m c] (Cont. (f (curryfn [v _] (Cont. c v))) c))))

(defn cont-t [inner]
  (let [i-return (:return inner)]
    (assoc cont-m
      :monadtrans {:lift (curryfn [m c] (run-monad inner (>>= m c)))}
      :inner inner)))

(defn run-cont [m]
  (loop [m m c identity]
    (let [m ((run-monad cont-m m) c)]
      (if (cont? m)
        (recur (get-cont m) (get-arg m))
        m))))

(defn run-c [c]
  (loop [c c f identity]
    (if (cont? c)
      (recur (get-cont c) (get-arg c))
      (f c))))

(defn reset [c]
  (return (run-cont c)))

(defn shift [f]
  (Returned.
   (fn [m]
     (fn [c]
       (run-cont (f #(run-cont (run-c (c %)))))))))

(defn run-cont-t [m comp cont]
  (let [comp ((run-monad m comp) cont)]
    (if (cont? comp)
      (recur m (get-cont comp) (get-arg comp))
      comp)))

(def t cont-t)
(def m cont-m)
