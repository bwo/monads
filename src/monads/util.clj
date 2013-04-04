(ns monads.util
  (:require [the.parsatron :as parsatron]
            [macroparser.functions :as functions]
            [macroparser.bindings :as bindings])
  (:use [monads.core :only [mzero >>= mdo return mplus]]))

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
  "Transform a function a -> b into a monadic function m a -> m b."
  ([f] #(lift-m f %))
  ([f m] (>>= m (comp return f))))

(defn sequence-m
  "Transform a sequence of monadic values [m a] into a monadic value
   which is a sequence, m [a]."
  [ms]
  (reduce (fn [m-acc m]
            (mdo mval <- m
                 ms <- m-acc
                 (return (conj ms mval))))
          (return ())
          (reverse ms)))

(defmacro deflift-m-n [n]
  (let [nm (symbol (str "lift-m-" n))
        arglists (map (comp vec #(cons 'f (for [n (range %)]
                                            (symbol (str "m" (inc n))))))
                      (range (inc n)))]
    `(defn ~nm
       {:arglists ~(list 'quote arglists)}
       ~@(for [passed-args (range n)]
           (let [f (gensym "f_")
                 argsyms (repeatedly passed-args #(gensym "now_"))]
             `([~f ~@argsyms]
                 (fn
                   ~@(for [next-args (range 1 (- (inc n) passed-args))]
                       (let [next-arg-syms (repeatedly next-args #(gensym "later_"))]
                         `([~@next-arg-syms] (~nm ~f ~@argsyms ~@next-arg-syms))))))))
       ~(let [f (gensym "f_")
              m-args (repeatedly n #(gensym "m_"))
              args (repeatedly n gensym)
              mdo (concat '[mdo]
                          (mapcat #(list %1 '<- %2) args m-args)
                          (list (list 'return (list* f args))))]
          `([~f ~@m-args]
              ~mdo)))))

(defmacro deflift-m-ns [lo hi]
  (when-not (== lo hi)
    `(do (deflift-m-n ~lo)
         (deflift-m-ns ~(inc lo) ~hi))))

(defn lift-m-2
  "As lift-m but for binary functions: transforms a -> b -> c into m a
   -> m b -> m c. Likewise for lift-m-3, etc."
  ([f] (fn
         ([x] (lift-m-2 f x))
         ([x y] (lift-m-2 f x y))))
  ([f m] #(lift-m-2 f m %))
  ([f m1 m2]
     (mdo a <- m1
          b <- m2
          (return (f a b)))))

(deflift-m-ns 3 9)

(def ^{:doc "Lift function application."} ap (lift-m-2 (fn [a b] (a b))))

(defn lift-m*
  ([f] (fn [& m-args] (apply lift-m* f m-args)))
  ([f & m-args]
      (mdo args <- (sequence-m m-args)
           (return (apply f args)))))

(defn fold-m
  "Analogous to reduce, except the result of f is in a monad: f is a -> b -> m a."
  [f acc xs]
  (if (empty? xs)
    (return acc)
    (mdo a <- (f acc (first xs))
         (fold-m f a (rest xs)))))

(defn msum
  "Add all the addends together using mplus."
  [addends]
  (reduce #(mplus %2 %1) (reverse addends)))

(defn mwhen
  "Execute the computation acc if p is truthy."
  [p acc]
  (if p
    acc
    (return nil)))

(defn guard
  "If p is truthy, return (return nil), otherwise mzero, halting the
  current computation."
  [p]
  (if p
    (return nil)
    mzero))

(defmacro lazy-pair [a b]
  `(lazy-seq (cons ~a (lazy-seq (cons ~b '())))))
