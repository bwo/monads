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

(defmacro monad [& {:as params}]
  `(let [params# (s/rename-keys ~params {:>>= :bind})]
     (assert (:bind params#) (str "monad " ~name " requires a bind operation!"))
     (assert (:return params#) (str "monad " ~name " requires a return operation!"))
     params#))

(defmacro defmonad [name & {:as params}]
  `(def ~name (monad ~@(apply concat params))))

(defmacro mdo [& exprs]
  `(mdo/mdo >>= ~@exprs))

(defmacro run-mdo [m & exprs]
  `(run-monad ~m (mdo ~@exprs)))

;;; monadplus
(def ^{:doc "The zero value for monadplus instances"}
  mzero (Returned. (fn [m] (-> m :monadplus :mzero))))

(defn mplus
  "Add the values of left and right. Required to be associative."
  [left right]
  (Mplus. left right))

;; monadfail
(defn mfail
  "Abort the current computation, with the message msg (if supported)."
  [msg]
  (Returned. (fn [m] ((-> m :monadfail :mfail) msg))))

(defn lift
  "Lift the computation inner up a level in the monad transformer stack."
  [inner]
  (Returned. (fn [m] ((-> m :monadtrans :lift) inner))))

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

;; monadstate
(def ^{:doc "Return the current state"}
  get-state
  (Returned. (fn [m] (-> m :monadstate :get-state))))
(defn put-state
  "Make the state be the value v."
  [v]
  (Returned. (fn [m] ((-> m :monadstate :put-state) v))))
(defn modify
  "Transform the current state by the function f, with extra args args."
  [f & args]
  (>>= get-state (comp put-state #(apply f % args))))

;;monadwriter
(defn tell
  "Add the value w to the log. Note that w must be a monoid."
  [w]
  (Returned. (fn [m] ((-> m :monadwriter :tell) w))))

(defn listen
  "Execute the computation comp, and return both its return value and
   the log it produces."
  [comp]
  (Returned. (fn [m] ((-> m :monadwriter :listen) comp))))

(defn pass
  "Execute the computation comp, which should return a value and a
   function, and return the value, applying the function to the log."
  [comp]
  (Returned. (fn [m] ((-> m :monadwriter :pass) comp))))

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
  (Returned. (fn [m] ((-> m :monaderror :throw-error) e))))

(defn catch-error
  "Try running the computation comp, calling the function handler if
   it is aborted by an error. The handler function will receive the
   error value as its argument."
  [comp handler]
  (Returned. (fn [m] ((-> m :monaderror :catch-error) comp handler))))

;; monadreader
(def ^{:doc "Return the current environment."}
  ask
  (Returned. (fn [m] (-> m :monadreader :ask))))

(defn local
  "Run the computation comp in an environment transformed by the function f."
  [f comp] (Returned. (fn [m] ((-> m :monadreader :local) f comp))))

(defn asks
  "Return the environment transformed by the function f, with extra args args."
  [f & args]
  (mdo x <- ask
       (return (apply f x args))))
