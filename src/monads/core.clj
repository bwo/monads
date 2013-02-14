(ns monads.core
  (:require [clojure.set :as s]
            [the.parsatron :as parsatron]
            [macroparser.functions :as functions]
            [macroparser.bindings :as bindings]
            [macroparser.monads :as parser]))

(deftype Return [v]
  Object
  (toString [this]
    (with-out-str (print v))))

(deftype Bind [m f]
  Object
  (toString [this]
    (with-out-str (print [m f]))))

(deftype MonadOp [path obj]
  Object
  (toString [this]
    (with-out-str (print [path obj]))))

(defn- monad-op? [o]
  (instance? MonadOp o))

(defn >> [m c]
  (Bind. m (fn [_] c)))

(defn return [x]
  (Return. x))

(defn- bind? [o]
  (instance? Bind o))

(defn- return? [o]
  (instance? Return o))

(defn >>= [m f]
  ;; may want to remove this micro-optimization since having it in
  ;; might obscure the broken-ness of broken monad impls
  (cond
   (return? m) (f (.v m))
   (return? f) m
   :else (Bind. m f)))

(defn run-monad [m computation]
  (cond
   (bind? computation)
   ;; Note: currently this creates nested calls equal to the number of
   ;; binds nested on the left. (In normal usage, however, this should
   ;; not be a big deal.)
   (recur m ((:bind m) (run-monad m (.m computation)) (.f computation)))
   (return? computation)
   ((:return m) (.v computation))
   (monad-op? computation)
   ((get-in m (.path computation)) (.obj computation))
   :else computation))

(defmacro monad [& {:as params}]
  `(let [params# (s/rename-keys ~params {:>>= :bind})]
     (assert (:bind params#) (str "monad " ~name " requires a bind operation!"))
     (assert (:return params#) (str "monad " ~name " requires a return operation!"))
     params#))

(defmacro defmonad [name & {:as params}]
  `(def ~name (monad ~@(apply concat params))))

;;; monadplus
(def mzero (MonadOp. [:monadplus :mzero] nil))
(defn mplus [left right]
  (MonadOp. [:monadplus :mplus] [left right]))

;; monadfail
(defn mfail [msg]
  (MonadOp. [:monadfail :mfail] msg))

(defn lift [m]
  (MonadOp. [:monadtrans :lift] m))

;; monadreader
(def ask (MonadOp. [:monadreader :ask] nil))
(defn asks [f] (MonadOp. [:monadreader :asks] f))
(defn local [f m] (MonadOp. [:monadreader :local] [f m]))

;; monadstate
(def get-state
  (MonadOp. [:monadstate :get-state] nil))

(defn put-state [v]
  (MonadOp. [:monadstate :put-state] v))

(defn modify [f] (>>= get-state (comp put-state f)))

;; monadcont

(defn callcc [f]
  (MonadOp. [:monadcont :callcc] f))

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

(defn lift-m [f]
  (fn [m]
    (>>= m (comp return f))))

(defn sequence-m [ms]
  (reduce (fn [m-acc m]
            (mdo mval <- m
                 ms <- m-acc
                 (return (conj ms mval))))
          (return [])
          ms))

(defn lift-m-2 [f]
  (fn [m1 m2]
    (mdo a <- m1
         b <- m2
         (return (f a b)))))

;; only works on curried fns
;; (run-monad maybe-m (ap (ap (return (curryfn #(+ %1 %2))) (return 1)) (return 2)))
;; #<Just 3>
(def ap (lift-m-2 (fn [a b] (a b))))

(defn lift-m* [f & m-args]
  (mdo args <- (sequence-m m-args)
       (return (apply f args))))

(defn fold-m [f acc xs]
  (if (empty? xs)
    (return acc)
    (mdo a <- (f acc (first xs))
         (fold-m f a (rest xs)))))

(defn guard [p acc]
  (if p
    acc
    (return nil)))

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
