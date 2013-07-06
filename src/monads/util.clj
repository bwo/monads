(ns monads.util
  (:require [the.parsatron :as parsatron]
            [macroparser.functions :as functions])
  (:use [monads.core :only [mzero >>= mdo return mplus]]))

(defmacro curryfn
  {:arglists (-> fn var meta :arglists)}
  [& args]
  (let [parsed (parsatron/run (functions/parse-fn-like) args)
        arities (:arities parsed)
        arity (:params (first arities))
        name (or (:name parsed) (gensym))
        parsed (assoc parsed :name name)
        n (count (:bindings arity))]
    (assert (== 1 (count arities)) "Can't curry multi-arity functions.")
    (assert (nil? (-> arity :bindings :rest)) "Can't curry functions with rest args")
    `(fn ~name
       ~@(for [now (range 1 n)]
           (let [nows (repeatedly now #(gensym "now-"))]
             `([~@nows]
                 (fn ~@(for [later-args (range 1 (- (inc n) now))]
                        (let [laters (repeatedly later-args #(gensym "later-"))]
                          `([~@laters] (~name ~@nows ~@laters))))))))
       ~(functions/unparse-arities arities))))

(defmacro defcurryfn
  {:arglists (-> defn var meta :arglists first list)}
  [& args]
  (let [parsed (parsatron/run (functions/parse-defn-like) args)
        attr-map (:attr-map parsed)
        docstring (:docstring parsed)
        arglists (->> parsed :arities first :params :bindings reverse
                      (iterate rest)
                      (take-while (comp not empty?))
                      (map reverse))
        meta-map (merge (meta (:name parsed))
                        attr-map
                        {:arglists (list 'quote (map vec arglists))}
                        (when docstring {:doc docstring}))
        as-fn (functions/unparse-fn-like (assoc parsed :type 'fn))]
    `(def ~(with-meta (:name parsed) meta-map) (curryfn ~@(rest as-fn)))))

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

(defmacro ^:private deflift-m-n [n]
  (let [nm (symbol (str "lift-m-" n))
        m-args (map #(symbol (str "m-" %)) (range 1 (inc n)))
        unwrapped-args (repeatedly n #(gensym))]
    `(defcurryfn ~nm [~'f ~@m-args]
       (mdo ~@(mapcat (fn [u m] [u '<- m]) unwrapped-args m-args)
            (return (~'f ~@unwrapped-args))))))

(defmacro ^:private deflift-m-ns [lo hi]
  (when-not (== lo hi)
    `(do (deflift-m-n ~lo)
         (deflift-m-ns ~(inc lo) ~hi))))

(defcurryfn lift-m-2
  "As lift-m but for binary functions: transforms a -> b -> c into m a
   -> m b -> m c. Likewise for lift-m-3, etc."
  [f m1 m2]
  (mdo a <- m1
       b <- m2
       (return (f a b))))

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
