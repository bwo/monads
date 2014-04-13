(ns monads.test-shiftreset
  (:require [monads.cont :as c])
  (:use monads.core
        monads.util
        [expectations :exclude [fail]]))


(def m- (lift-m-2 -))
(def m+ (lift-m-2 +))
(def m* (lift-m-2 *))

(expect 15
        (c/run-cont (m- (c/reset
                         (m+ (return 3)
                             (c/shift (fn [k] (return (k (k 10)))))))
                        (return 1))))

(expect 9
        (c/run-cont (m- (c/reset
                         (m+ (return 3)
                             (c/shift (fn [k] (return 10)))))
                        (return 1))))

(expect 5
        (c/run-cont (m- (c/reset
                         (m+ (return 3) (return 3)))
                        (return 1))))


(defn complete [d]
  (letfn [(go [d n]
            (if (zero? d)
              :empty
              {:left (go (dec d) (* 2 n))
               :n n
               :right (go (dec d) (inc (* 2 n)))}))]
    (go d 1)))

(defn walk-tree [t]
  (letfn [(yield [n]
            (c/shift (fn [k] (return {:k k :n n}))))]
    (if (= :empty t)
      (return ())
      (mdo (walk-tree (:left t))
           let _ = (println "walked to" (:n t))
           (yield (:n t))
           (walk-tree (:right t))))))

(defn w [t]
  (letfn [(go [t]
            (if (= :done t)
              ()
              (do (println "visited" (:n t))
                  (recur (c/run-c (:k t))))))]
    (go (c/run-cont (mdo (walk-tree t)
                       (return :done))))))

(expect [["walked to" 4]
         ["visited" 4]
         ["walked to" 2]
         ["visited" 2]
         ["walked to" 5]
         ["visited" 5]
         ["walked to" 1]
         ["visited" 1]
         ["walked to" 6]
         ["visited" 6]
         ["walked to" 3]
         ["visited" 3]
         ["walked to" 7]
         ["visited" 7]]
        (side-effects [println]
                      (w (complete 3))))

;;; note that lift-m* does not work! (or, more precisely, sequence-m doesn't).
