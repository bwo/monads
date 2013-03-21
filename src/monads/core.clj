(ns monads.core
  (:require [monads.types :as types]
            [clojure.set :as s]
            [the.parsatron :as parsatron]
            [macroparser.bindings :as bindings]
            [macroparser.monads :as parser])
  (:import [monads.types Return Returned Bind Pair]))

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

;;; monadplus
(def mzero (Returned. (fn [m] (-> m :monadplus :mzero))))
(defn mplus [left right]
  (Returned. (fn [m] ((-> m :monadplus :mplus) [left right]))))

;; monadfail
(defn mfail [msg]
  (Returned. (fn [m] ((-> m :monadfail :mfail) msg))))

(defn lift [inner]
  (Returned. (fn [m] ((-> m :monadtrans :lift) inner))))

;; monadstate
(def get-state
  (Returned. (fn [m] (-> m :monadstate :get-state))))
(defn put-state [v]
  (Returned. (fn [m] ((-> m :monadstate :put-state) v))))
(defn modify [f] (>>= get-state (comp put-state f)))

;;monadwriter
(defn tell [w]
  (Returned. (fn [m] ((-> m :monadwriter :tell) w))))

(defn listen [comp]
  (Returned. (fn [m] ((-> m :monadwriter :listen) comp))))

;; haskell dox:
;; | @'pass' m@ is an action that executes the action @m@, which returns
;; a value and a function, and returns the value, applying the function
;; to the output.
(defn pass [comp]
  (Returned. (fn [m] ((-> m :monadwriter :pass) comp))))

(defn listens [f m]
  (mdo ^Pair p <- (listen m)
       (return (types/fst p) (f (types/snd p)))))

(defn censor [f m]
  (pass (mdo a <- m
             (return [a f]))))

;; monaderror
(defn throw-error [e]
  (Returned. (fn [m] ((-> m :monaderror :throw-error) e))))
(defn catch-error [comp handler]
  (Returned. (fn [m] ((-> m :monaderror :catch-error) comp handler))))
