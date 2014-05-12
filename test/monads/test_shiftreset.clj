(ns monads.test-shiftreset
  (:require [monads.cont :as c]
            [monads.state :as s])
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

(defn with8 [m]
  (s/eval-state m 8))

;; *ContExample S> let with8 x = S.evalState x 8
;; *ContExample S> let ex1 = liftM2 (+) (return 1) (resetT $ liftM2 (*) (return 2) (shiftT $ \\k ->  return $ with8 $  k $ with8 ((k 10)>> get))) in S.runState (runCT ex1) 4
;; (17,4)
;; *ContExample S> let ex1 = liftM2 (+) (return 1) (resetT $ liftM2 (*) (return 2) (shiftT $ \\k ->  return $ with8 $  k $ with8 ((k 10)))) in S.runState (runCT ex1) 4
;; (41,4)

(expect 17 (s/eval-state
            (c/run-cont-t (c/t s/m)
                          (m+ (c/reset
                               (m* (return 2)
                                   (c/shift (fn [k] (return (with8 (k (with8 (>> (k 10) get-state)))))))))
                              (return 1)))
            4))

(expect 200 (c/run-cont (c/reset
                         (m* (return 2)
                             (c/shift (fn [k] (let [x (k 10)]
                                               (return (if (> x 15)
                                                         (k 100)
                                                         (k -1))))))))))

(expect 41 (s/eval-state
            (c/run-cont-t (c/t s/m)
                          (m+ (c/reset
                               (m* (return 2)
                                   (c/shift (fn [k] (return (with8 (k (with8 (k 10)))))))))
                              (return 1)))
            4))

(expect 9
        (c/run-cont (m- (c/reset
                         (m+ (return 3)
                             (c/shift (fn [k] (return 10)))))
                        (return 1))))

(expect 9 (s/eval-state
           (c/run-cont-t (c/t s/m)
                         (m- (c/reset
                              (m+ (return 3)
                                  (c/shift (fn [k] (return 10)))))
                             (return 1)))
           1))

(expect 5
        (c/run-cont (m- (c/reset
                         (m+ (return 3) (return 3)))
                        (return 1))))

(expect 5 (s/eval-state
           (c/run-cont-t (c/t s/m)
                         (m- (c/reset (m+ (return 3) (return 3))) (return 1))) 3))

(expect [-1 10012]
        (seq (s/run-state
              (c/run-cont-t (c/t s/m)
                            (c/callcc (fn [k]
                                        (fold-m (fn [acc x]
                                                  (if (> acc 10000)
                                                    (mdo (lift (put-state acc))
                                                         (k -1))
                                                    (return (+ acc x))))
                                                1
                                                (range 1 1300)))))
              3)))

(expect 8386
        (s/eval-state
         (c/run-cont-t (c/t s/m)
                       (c/callcc (fn [k]
                                   (fold-m (fn [acc x]
                                             (if (> acc 10000)
                                               (k -1)
                                               (return (+ acc x))))
                                           1
                                           (range 1 130)))))
         3))

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

(defn restart [t]
  (println "visited" (:n t))
  ((:k t) ()))

(defn w [t]
  (letfn [(go [t]
            (if (= :done t)
              ()
              (recur (restart t))))]
    (go (c/run-cont (mdo (walk-tree t)
                         (return :done))))))

(defn w-t [t]
  (letfn [(go [t]
            (if (= :done t)
              (return ())
              (>>= (restart t) go)))]
    (s/run-state (>>= (c/run-cont-t (c/t s/m) (mdo (walk-tree t)
                                                   (return :done)))
                      go) 4)))

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
                      (w-t (complete 3))))

(defn same-fringe? [t1 t2]
  (let [start #(c/run-cont (mdo (walk-tree %) (return :done)))]
    (loop [s1 (start t1) s2 (start t2)]
      (cond
       (and (= :done s1) (= :done s2)) true
       (= :done s1) (do (println "t1 done, t2" (:n s2)) false)
       (= :done s2) (do (println "t2 done, t1" (:n s1)) false)
       (= (:n s1) (:n s2)) (recur ((:k s1) ()) ((:k s2) ()))
       :else (do (println "t1" (:n s1) "t2" (:n s2)) false)))))

(expect [["walked to" 4]
         ["walked to" 8]
         ["t1" 4 "t2" 8]]
        (side-effects [println]
                      (same-fringe? (complete 3) (complete 4))))

(expect (same-fringe? (complete 3) (complete 3)))

(expect -1 (c/run-cont (c/callcc (fn [k]
                                   (fold-m (fn [acc x]
                                             (if (zero? x)
                                               (k -1)
                                               (return (+ acc x))))
                                           1
                                           (concat (range 1 1000)
                                                   (range)))))))

(expect 3
        (c/run-cont  (reduce (fn [acc _] (c/callcc (fn [k] acc))) (return 3) (range 100000))))
