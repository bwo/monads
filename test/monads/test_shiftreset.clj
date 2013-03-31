(ns monads.test-shiftreset
  (:require [monads.cont :as c])
  (:use monads.core
        monads.util
        expectations))


(def m- (lift-m-2 -))
(def m+ (lift-m-2 +))
(def m* (lift-m-2 *))

(expect 15
        (c/run-cont (m- (c/reset
                         (m+ (return 3)
                             (c/shift (fn [k] (return (k (k 10))))))) (return 1))))

(expect 9
        (c/run-cont (m- (c/reset
                         (m+ (return 3)
                             (c/shift (fn [k] (return 10))))) (return 1))))

(expect 5
        (c/run-cont (m- (c/reset
                         (m+ (return 3) (return 3))) (return 1))))

;;; note that lift-m* does not work! (or, more precisely, sequence-m doesn't).
