(ns monads.test-tree
  (:require [monads.examples.treenumber :as tn]
            [monads.state :as s]
            [monads.list :as l]
            [monads.error :as e]
            [monads.maybe :as m]
            [monads.reader :as r]
            [monads.types :as t])
  (:use [expectations :exclude [fail]]
        [monads.core :exclude [fail]]))

(def numbered (tn/node 0
                       (tn/node 0 nil (tn/node 1
                                               (tn/node 2 nil (tn/node 0 nil nil))
                                               (tn/node 1
                                                        nil
                                                        (tn/node 3 nil nil))))
                       (tn/node 3
                                (tn/node 1 nil nil)
                                (tn/node 2
                                         (tn/node 4
                                                  (tn/node 0 nil nil)
                                                  nil)
                                         nil))))


;; if we just (expect numbered ...), expectations complains:
           ;; expected: {:val 0, :left {:val 0, :left nil, :right {:val 1, :left {:val 2, :left nil, :right {:val 0, :left nil, :right nil}}, :right {:val 1, :left nil, :right {:val 3, :left nil, :right nil}}}}, :right {:val 3, :left {:val 1, :left nil, :right nil}, :right {:val 2, :left {:val 4, :left {:val 0, :left nil, :right nil}, :right nil}, :right nil}}}
           ;;      was: {:val 0, :left {:val 0, :left nil, :right {:val 1, :left {:val 2, :left nil, :right {:val 0, :left nil, :right nil}}, :right {:val 1, :left nil, :right {:val 3, :left nil, :right nil}}}}, :right {:val 3, :left {:val 1, :left nil, :right nil}, :right {:val 2, :left {:val 4, :left {:val 0, :left nil, :right nil}, :right nil}, :right nil}}}

           ;; in expected, not actual: null
           ;; in actual, not expected: null
;; useful!

(expect (= numbered (tn/num-tree tn/a-tree)))

(expect (= numbered
           (t/from-just (s/eval-state-t (s/t m/m) (tn/number-tree tn/a-tree) {}))))

(expect (= numbered
           (t/from-right (s/eval-state-t (s/t e/m) (tn/number-tree tn/a-tree) {}))))

(expect (= numbered
           (t/from-just
            (t/from-just
             (s/eval-state-t (s/t (m/t m/m)) (tn/number-tree tn/a-tree) {})))))

(expect (= numbered
           (t/from-just
            (t/from-right
             (s/eval-state-t (s/t (m/t e/m)) (tn/number-tree tn/a-tree) {})))))

(expect (list numbered)
        (s/eval-state-t (s/t l/m) (tn/number-tree tn/a-tree) {}))


(expect (list numbered)
        (map (comp t/from-just t/from-right)
             (r/run-reader-t
              (r/t l/m)
              (s/eval-state-t (s/t (m/t (e/t (r/t l/m)))) (tn/number-tree tn/a-tree) {}) 2)))
