(ns monads.examples.interp
  (:require [clojure.string :as str]
            [monads.cont :as cont]
            [monads.core :refer :all]
            [monads.error :as error]
            [monads.rws :as rws]            
            [monads.types :as types]
            [monads.util :as u]))

(defn lookup [m k]
  (if (contains? m k)
    (return (get m k))
    mzero))

(defn lookups
  ([k] (fn [& ms] (apply lookups k ms)))
  ([k & ms]
     (u/msum (map #(lookup % k) ms))))

(defn get-var [sym]
  (catch-error (-> (lookups sym)
                   (u/lift-m-2 (lift ask) (lift get-state))
                   join)
               (constantly (throw-error (str "no value for symbol " sym)))))

(defn check! [b errmsg]
  (u/mwhen (not b) (throw-error errmsg)))

(declare m-eval)

(defn do- [exprs]
  (lift-m last (u/map-m m-eval exprs)))

(defn define- [args]
  (mdo (check! (== 2 (count args)) "define takes two arguments")
       (check! (symbol? (first args)) "first argument to define must be a symbol")
       (>>= (m-eval (second args)) (partial (comp lift modify) assoc (first args)))))

(u/defcurryfn extend-env [bindings bound env]
  (reduce (fn [acc [k v]] (assoc acc k v)) env (map vector bindings bound)))

(defn let- [args]
  (mdo (check! (list? (first args)) "let requires a list for its first argument")
       (check! (even? (count (first args))) "let requires an even number of bindings")
       let [bindings (take-nth 2 (first args))
            exprs (take-nth 2 (rest (first args)))
            let-expr (list* 'do (rest args))]
       (check! (every? symbol? bindings) "let bindings must be symbols")
       bound <- (u/map-m m-eval exprs)
       (local (extend-env bindings bound)
              (m-eval let-expr))))

(defn if- [args]
  (mdo (check! (#{2 3} (count args)) "if requires either two or three arguments")
       tst <- (m-eval (first args))
       (if (and tst (not= tst '()))
         (m-eval (second args))
         (u/mwhen (== 3 (count args))
                  (m-eval (nth args 2))))))

(defn lambda- [args]
  (mdo (check! (list? (first args)) "lambda requires a list for its first argument")
       (check! (every? symbol? (first args)) "lambda can only bind symbols")
       my-env <- (lift ask)
       (return {:formals (first args)
                :body (list* 'do (rest args))
                :env my-env})))

(def specials {'define define-
               'let let-
               'do do-
               'if if-
               'lambda lambda-})

(defn arity-error [name supplied formal]
  (str "arity mismatch: " name " applied to " supplied " arguments, but takes "
       formal))

(defn m-apply [op args]
  (if-let [special (specials op)]
    (special args)
    (mdo eop <- (m-eval op)
         (if (fn? eop)
           (>>= (u/map-m m-eval args) (partial apply eop))
           (mdo (check! (:formals eop) (str op " is not a function"))
                (check! (== (count args) (count (:formals eop)))
                        (arity-error op (count args) (count (:formals eop))))
                eargs <- (u/map-m m-eval args)
                (local (fn [_] (extend-env (:formals eop) eargs (:env eop)))
                       (m-eval (:body eop))))))))

(defn m-eval [o]
  (cond
   (symbol? o) (get-var o)
   (seq? o) (if (empty? o) (return ()) (m-apply (first o) (rest o)))
   :else (return o)))

(defmacro op [f pred]
  (let [predstr (apply str (concat (butlast (str pred))))
        name (str f)]
    `(fn [& args#]
       (mdo (check! (every? ~pred args#) ~(str name " arguments must be " predstr))
            (return (apply ~f args#))))))

(defn check-args! [name arity args & [type typename]]
  (mdo (check! (== (count args) arity) (arity-error name (count args) arity))
       (u/mwhen type
         (check! (every? type args) (str name " argument(s) must be " typename "(s)")))))

(def starting-global-env
  {'+ (op + number?)
   '- (op - number?)
   '* (op * number?)
   '/ (op / number?)
   '% (fn [& args] (mdo (check-args! "%" 2 args number? "number")
                       (return (mod (first args) (second args)))))
   'nil? (fn [& args] (mdo (check-args! "nil?" 1 args)
                          (return (identical? (first args) ()))))
   'zero? (fn [& args] (mdo (check-args! "zero?" 1 args number? "number")
                           (return (zero? (first args)))))
   'print (fn [& args] (lift (tell [(map #(if (map? %) "<fn>" %) args)])))})

(defn run [exprs]
  (let [r (run-monad (error/t (rws/t cont/m)) (do- exprs))]
    (->> (rws/run-rws-t (rws/t cont/m) r starting-global-env {'true true 'false false})
         cont/run-cont
         ((juxt types/fst types/thd)))))


(run '((define d (lambda (c) (lambda (d) (d c))))
       (define even? (lambda (n) (if (zero? n) true (odd? (- n 1)))))
       (define odd? (lambda (n) (if (zero? n) false (even? (- n 1)))))
       (let (d (d 3))
         (print d (d (lambda (c) ((if (even? c) * +) c 4))) "\n")
         (d (lambda (c) ((if (odd? c) * +) c 4))))))

(run '((define cons (lambda (a b) (lambda (m) (m a b))))
       (define car (lambda (pair) (pair (lambda (a b) a))))
       (define cdr (lambda (pair) (pair (lambda (a b) b))))
       (define foldl (lambda (f acc lst)
                             (if (nil? lst)
                               acc
                               (foldl f (f acc (car lst)) (cdr lst)))))
       (define sum (lambda (lst) (foldl + 0 lst)))
       (sum (cons 1 (cons 2 (cons 4 ()))))))
