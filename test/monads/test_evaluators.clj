(ns monads.test-evaluators
  (:require [monads.examples.evaluator1 :as e1]
            [monads.examples.evaluator2 :as e2]
            [monads.examples.evaluator3 :as e3]
            [monads.examples.evaluator4 :as e4]
            [monads.examples.evaluator5 :as e5]
            [monads.types :as t])
  (:use expectations))

(expect [-3.9301414676796522 {'pi 3.141592, 'e 2.718281, 'm -3.6566684960143307, 'deg 57.295779}]
        (seq (e1/evaluate '((m = (pi * (2 / (1 - e)))) + (1 / m)))))

(expect  [[-3.9301414676796522 {'pi 3.141592, 'e 2.718281, 'm -3.6566684960143307, 'deg 57.295779}] ["plus: (m = (pi * (2 / (1 - e)))), (1 / m)" "times: pi, (2 / (1 - e))" "div: 2, (1 - e)" "minus: 1, e" "decl m" "div: 1, m"]]
        (map seq (e2/evaluate '((m = (pi * (2 / (1 - e)))) + (1 / m)))))

(expect [[-3.9301414676796522 {'pi 3.141592, 'e 2.718281, 'm -3.6566684960143307, 'deg 57.295779}] ["plus: (m = (pi * (2 / (1 - e)))), (1 / m)" "times: pi, (2 / (1 - e))" "div: 2, (1 - e)" "minus: 1, e" "decl m" "div: 1, m"]]
        (map seq (t/from-just (e3/evaluate '((m = (pi * (2 / (1 - e)))) + (1 / m))))))

(expect nil (e3/evaluate '((m = (pi * (2 / (1 - e)))) + (1 / a))))

(expect "no value for name: a"
        (t/from-left (e4/evaluate '((m = (pi * (2 / (1 - e)))) + (1 / a)))))

(expect
 ["no value for name: a"
  ["plus: (pi - 1), a"
   "minus: pi, 1"]]
 (let [r (e5/evaluate '((pi - 1) + a))]
   [(t/from-left (first r)) (second r)]))

(expect
 [[4.859873 {'pi 3.141592, 'e 2.718281, 'deg 57.295779}]
  ["plus: (pi - 1), e"
   "minus: pi, 1"]]
 (let [r (e5/evaluate '((pi - 1) + e))]
   [(seq (t/from-right (first r))) (second r)]))
