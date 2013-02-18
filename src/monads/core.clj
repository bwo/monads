(ns monads.core
  (:require [clojure.set :as s]
            [the.parsatron :as parsatron]
            [macroparser.functions :as functions]
            [macroparser.bindings :as bindings]
            [macroparser.monads :as parser]))

(set! *warn-on-reflection* true)

(declare run-monad)

(defprotocol MRun
  (mrun [this m]))

(extend-protocol MRun
  Object
  (mrun [this _] this)
  nil
  (mrun [this _] nil))

(deftype Return [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] ((:return m) v)))

(deftype Returned [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (v m)))

(deftype Bind [comp f]
  Object
  (toString [this]
    (with-out-str (print [comp f])))
  MRun
  (mrun [_ m]  ((:bind m) (run-monad m comp) f)))

(defn return [x]
  (Return. x))

(defn >>= [m f]
  (Bind. m f))

(defn >> [m c]
  (>>= m (fn [_] c)))

(defn run-monad [m computation]
  (mrun computation m))

(defmacro monad [& {:as params}]
  `(let [params# (s/rename-keys ~params {:>>= :bind})]
     (assert (:bind params#) (str "monad " ~name " requires a bind operation!"))
     (assert (:return params#) (str "monad " ~name " requires a return operation!"))
     params#))

(defmacro defmonad [name & {:as params}]
  `(def ~name (monad ~@(apply concat params))))

;;; monadplus
(def mzero (Returned. (fn [m] (-> m :monadplus :mzero))))
(defn mplus [left right]
  (Returned. (fn [m] ((-> m :monadplus :mplus) [left right]))))

;; monadfail
(defn mfail [msg]
  (Returned. (fn [m] ((-> m :monadfail :mfail) msg))))

(defn lift [inner]
  (Returned. (fn [m] ((-> m :monadtrans :lift) inner))))
;;; utils

(defn- unparse-m-expr [inside outside]
  (case (:type outside)
    :let `(let [~@(mapcat (fn [{:keys [bound expr]}] [(bindings/unparse-bindings bound) expr])
                          (:bindings outside))]
            ~inside)
    (:normal :bind) `(>>= ~(:expr outside) (fn [~(bindings/unparse-bindings (:bound outside))]
                                             ~inside))))

(defmacro mdo [& exprs]
  (let [parsed (reverse (parsatron/run (parser/parse-mdo) exprs))]
    (assert (= :normal (:type (first parsed))) "Last expression in mdo must be a normal clojure expression.")
    (reduce unparse-m-expr (:expr (first parsed)) (rest parsed))))

(defmacro run-mdo [m & exprs]
  `(run-monad ~m (mdo ~@exprs)))

(defn lift-m
  ([f] #(lift-m f %))
  ([f m] (>>= m (comp return f))))

(defn sequence-m [ms]
  (reduce (fn [m-acc m]
            (mdo mval <- m
                 ms <- m-acc
                 (return (conj ms mval))))
          (return [])
          ms))

(defn lift-m-2
  ([f] #(lift-m-2 f %))
  ([f m] #(lift-m-2 f m %))
  ([f m1 m2]     
     (mdo a <- m1
          b <- m2
          (return (f a b)))))

;; only works on curried fns, alas
;; (run-monad maybe-m (ap (ap (return (curryfn #(+ %1 %2))) (return 1)) (return 2)))
;; #<Just 3>
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
