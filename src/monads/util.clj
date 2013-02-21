(ns monads.util
  (:require [the.parsatron :as parsatron]
            [macroparser.functions :as functions]
            [macroparser.bindings :as bindings])
  (:use [monads.core :only [mzero >>= mdo return]]))

(defmacro curryfn [& args]
  (let [parsed (parsatron/run (functions/parse-fn-like)
                              (if (and (== 1 (count args))
                                       (#{'fn 'fn*} (ffirst args)))
                                (rest (first args))
                                args))
        arities (:arities parsed)
        arity (first arities)
        bindings (map bindings/unparse-bindings (:bindings (:params arity)))
        body (reduce (fn [acc binding]
                       `(fn [~binding] ~acc))
                     `(do ~@(:body arity))
                     (reverse bindings))]
    (assert (== 1 (count arities)) "Can't curry multi-arity functions")
    (assert (nil? (-> arity :bindings :rest)) "Can't curry functions with rest args")
    body))


(defn lift-m
  ([f] #(lift-m f %))
  ([f m] (>>= m (comp return f))))

(defn sequence-m [ms]
  (reduce (fn [m-acc m]
            (mdo mval <- m
                 ms <- m-acc
                 (return (conj ms mval))))
          (return ())
          (reverse ms)))

(defn lift-m-2
  ([f] #(lift-m-2 f %))
  ([f m] #(lift-m-2 f m %))
  ([f m1 m2]     
     (mdo a <- m1
          b <- m2
          (return (f a b)))))

(def ap (lift-m-2 (fn [a b] (a b))))

(defn lift-m*
  ([f] (fn [& m-args] (apply lift-m* f m-args)))
  ([f & m-args]
      (mdo args <- (sequence-m m-args)
           (return (apply f args)))))

(defn fold-m [f acc xs]
  (if (empty? xs)
    (return acc)
    (mdo a <- (f acc (first xs))
         (fold-m f a (rest xs)))))

(defn mwhen [p acc]
  (if p
    acc
    (return nil)))

(defn guard [p]
  (if p
    (return nil)
    mzero))

(defmacro lazy-pair [a b]
  `(lazy-seq (cons ~a (lazy-seq (cons ~b '())))))

(defmacro if-inner-return [m ifb elseb]
  `(if-let [~'i-return (-> ~m :inner :return)]
     ~ifb
     ~elseb))
