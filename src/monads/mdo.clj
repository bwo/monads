(ns monads.mdo
  (:refer-clojure :exclude [symbol char map vector list keyword seq])
  (:use [the.parsatron :exclude [string]]
        [macroparser.parsers])
  (:require [macroparser.bindings :as bindings]
            [clojure.core :as clj]))

(defparser monadic-bind []
  (let->> [bound (bindings/binding-form-simple)
           _ (symbol '<-)
           expr (expression)]
          (always {:bound bound :expr expr :type :bind})))

(defparser let-expression []
  (lift #(do {:type :let :bindings %})
        (>> (symbol 'let)
            (choice+
             (>>1 (lift (fn [bvec]
                          (clj/map (fn [[bound expr]] {:bound bound :expr expr}) bvec))
                        (vector (many1 (bindings/binding-pair))))
                  (lookahead (anything-but '=)))
             (many1 (attempt (let->> [bound (bindings/binding-form-simple)
                                      _ (symbol '=)
                                      expr (expression)]
                                     (always {:bound bound :expr expr}))))))))

(defparser normal-expression []
  (lift (fn [expr] {:bound (gensym) :expr expr :type :normal})
        (>>1 (anything-but 'let)
             (lookahead (either (eof) (anything-but '<-))))))

(defparser parse-mdo []
  (>>1 (many (choice+ (normal-expression) (monadic-bind) (let-expression))) (eof)))

(defn unparse-m-expr [binder inside outside]
  (case (:type outside)
    :let `(let [~@(mapcat (fn [{:keys [bound expr]}] [(bindings/unparse-bindings bound) expr])
                          (:bindings outside))]
            ~inside)
    (:normal :bind) `(~binder ~(:expr outside)
                              (fn [~(bindings/unparse-bindings (:bound outside))]
                                ~inside))))

(defmacro mdo [binder & exprs]
  (let [parsed (reverse (run (parse-mdo) exprs))]
    (assert (= :normal (:type (first parsed))) "Last expression in mdo must be a normal clojure expression.")
    (reduce #(unparse-m-expr binder %1 %2) (:expr (first parsed)) (rest parsed))))
