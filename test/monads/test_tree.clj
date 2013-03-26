(ns monads.test-tree
  (:require [monads.examples.treenumber :as tn]
            [monads.state :as s]
            [monads.list :as l]
            [monads.error :as e]
            [monads.maybe :as m]
            [monads.reader :as r]
            [monads.types :as t])
  (:use expectations
        monads.core))

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


(expect numbered (tn/num-tree tn/a-tree))

;; for some reason, omitting the from-just and expecting (t/just
;; numbered) doesn't work.
(expect numbered
        (t/from-just (s/eval-state-t (s/t m/m) (tn/number-tree tn/a-tree) {})))

(expect numbered
        (t/from-right (s/eval-state-t (s/t e/m) (tn/number-tree tn/a-tree) {})))

(expect numbered
        (t/from-just
         (t/from-just
          (s/eval-state-t (s/t (m/t m/m)) (tn/number-tree tn/a-tree) {}))))

(expect numbered
        (t/from-just
         (t/from-right
          (s/eval-state-t (s/t (m/t e/m)) (tn/number-tree tn/a-tree) {}))))

(expect (list numbered)
        (s/eval-state-t (s/t l/m) (tn/number-tree tn/a-tree) {}))


(expect (list numbered)
        (map (comp t/from-just t/from-right)
             (r/run-reader-t
              (r/t l/m)
              (s/eval-state-t (s/t (m/t (e/t (r/t l/m)))) (tn/number-tree tn/a-tree) {}) 2)))
