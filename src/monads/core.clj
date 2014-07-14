(ns monads.core
  (:require [monads.mdo :as mdo]
            [monads.types :as types]
            [clojure.algo.generic.functor :as f]
            [clojure.set :as s])
  (:use [monads.types :only [if-instance]])
  (:import [monads.types Return Returned Bind Pair Mplus]))

(set! *warn-on-reflection* true)

(defn return [x]
  (Return. x))

(defn >>= [m f]
  (Bind. m f))

(defn >> [m c]
  (>>= m (fn [_] c)))

(defn run-monad [m computation]
  (types/mrun computation m))

(defmacro reify+
  "Like reify, except that one can conditionally decide what
   interfaces to implement, using \"when\".

   The syntax of reify is extended to allow both

   Protocol
   (method [] ...)

   lines and also

   (when condition
      Protocol2
      (method2 [] ...))

   If, at runtime, condition is truthy, the reified value will also
   support Protocol2."
  [& signatures]
  (let [when? (fn [f] (and (seq? f)
                          (symbol? (first f))
                          (= #'when (resolve (first f)))))
        whens (filter when? signatures)
        signatures (remove when? signatures)
        reified (gensym "reified")]
    `(let [~reified (reify ~@signatures)]
       ~@(for [wh whens]
           `(when ~(second wh)
              (extend-type (class ~reified)
                ~@(drop 2 wh))))
       ~reified)))

(defmacro monad [& params]
  (let [return (first (filter #(and (seq? %) (= 'mreturn (first %))) params))
        bind (first (filter #(and (seq? %) (= 'bind (first %))) params))
        params (remove #(and (seq? %) (#{'mreturn 'bind} (first %))) params)]
    (assert bind "monad cannot be defined without bind!")
    (assert return "monad cannot be defined without mreturn!")
    `(reify+ types/Monad ~return ~bind ~@params)))

(defmacro defmonad [name & params]
  `(def ~name (monad ~@params)))

(defmacro mdo
  "Special syntax for monadic computations. An `mdo` form contains one of three kinds of elements:

   - binding elements, which have the form `destructure <- expression`;

   - plain elements, which are just expressions (except that they
     cannot consist solely of the symbol `<-` or the symbol `let`;

   - let elements, which can be either

        let destructure1 = expression1
            destructure2 = expression2
            ...

     or

        let [destructure1 expression1
             destructure2 expression2
             ...]

   `mdo` forms must end with a plain element.

   These are rewritten into clojure.core/let expressions, and binding
   using >>= and anonymous functions. For instance (from
   treenumber.clj in examples):

   (mdo num <- (number-node val)
        nt1 <- (number-tree left)
        nt2 <- (number-tree right)
        let [new-node (node num nt1 nt2)]
        (return new-node))

   becomes

   (>>= (number-node val)
        (fn [num] (>>= (number-tree left)
                       (fn [nt1] (>>= (number-tree right)
                                     (fn [nt2] (let [new-node (node num nt1 nt2)]
                                                     (return new-node))))))))

   Note that plain elements can include arbitrary Clojure control or
   binding expressions (so let expressions don't actually need to be
   baked in); the above could also have been:
 
   (mdo num <- (number-node val)
        nt1 <- (number-tree left)
        nt2 <- (number-tree right)
        (let [new-node (node num nt1 nt2)]
             (return new-node)))"
  [& exprs]
  `(mdo/mdo >>= ~@exprs))

(defmacro run-mdo [m & exprs]
  `(run-monad ~m (mdo ~@exprs)))

(defn join [m] (>>= m identity))

(defn lift-m
  "Transform a function a -> b into a monadic function m a -> m b."
  ([f] #(lift-m f %))
  ([f m] (>>= m (comp return f))))

(defmacro ^:private define-fmaps [types]
  (when (seq types)
    `(do (defmethod f/fmap ~(first types) [f# o#]
           (lift-m f# o#))
         (define-fmaps ~(rest types)))))

(define-fmaps [Return Mplus Returned Bind])

;;; monadplus
(def ^{:doc "The zero value for monadplus instances"}
  mzero (Returned. (fn [m] (types/mzero m))))

(defn mplus
  "Add the values of left and right. Required to be associative."
  [left right]
  (Mplus. left right))

;; monadfail
(defn fail
  "Abort the current computation, with the message msg (if supported)."
  [msg]
  (Returned. (fn [m] (types/fail m msg))))

;; monadtrans
(defn lift
  "Lift the computation inner up a level in the monad transformer stack."
  [inner]
  (Returned. (fn [m] (types/lift m inner))))


;; monadstate
(def ^{:doc "Return the current state"}
  get-state
  (Returned. (fn [m] (types/get-state m))))
(defn put-state
  "Make the state be the value v."
  [v]
  (Returned. (fn [m] (types/put-state m v))))
(defn modify
  "Transform the current state by the function f, with extra args args."
  [f & args]
  (>>= get-state (comp put-state #(apply f % args))))

;;monadwriter
(defn tell
  "Add the value w to the log. Note that w must be a monoid."
  [w]
  (Returned. (fn [m] (types/tell m w))))

(defn listen
  "Execute the computation comp, and return both its return value and
   the log it produces."
  [comp]
  (Returned. (fn [m] (types/listen m comp))))

(defn pass
  "Execute the computation comp, which should return a value and a
   function, and return the value, applying the function to the log."
  [comp]
  (Returned. (fn [m] (types/pass m comp))))

(defn listens
  "Execute the computation m, adding the result of calling f on its log to
  the its return value."
  [f m]
  (mdo p <- (listen m)
       (return [(first p) (f (second p))])))

(defn censor
  "Execute the computation m, returning the value it returns and modifying
  its log by the function f."
  [f m]
  (pass (mdo a <- m
             (return [a f]))))

;; monaderror
(defn throw-error
  "Abort the current computation, with the error e."
  [e]
  (Returned. (fn [m] (types/throw-error m e))))

(defn catch-error
  "Try running the computation comp, calling the function handler if
   it is aborted by an error. The handler function will receive the
   error value as its argument."
  [comp handler]
  (Returned. (fn [m] (types/catch-error m comp handler))))

;; monadreader
(def ^{:doc "Return the current environment."}
  ask
  (Returned. (fn [m] (types/ask m))))

(defn local
  "Run the computation comp in an environment transformed by the function f."
  [f comp] (Returned. (fn [m] (types/local m f comp))))

(defn asks
  "Return the environment transformed by the function f, with extra args args."
  [f & args]
  (mdo x <- ask
       (return (apply f x args))))
