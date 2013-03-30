(ns monads.test-fail
  (:require [monads.maybe :as m]
            [monads.error :as e]
            [monads.state :as s]
            [monads.writer :as w]
            [monads.reader :as r]
            [monads.types :as t]
            [monads.list :as l])
  (:use expectations
        monads.core))

(defn safe-div [num denom]
  (if (== 0 denom)
    (mfail "zero denominator!")
    (return (/ num denom))))

(expect nil (run-monad m/m (safe-div 4 0)))
(expect nil (run-monad l/m (safe-div 4 0)))
(expect "zero denominator!" (t/from-left (run-monad e/m (safe-div 5 0))))

(expect "zero denominator!" (t/from-left
                             (s/run-state-t (s/t e/m)
                                            (mdo x <- get-state
                                                 (lift (safe-div 2 x)))
                                            0)))
(expect "zero denominator!" (t/from-left
                             (s/run-state-t (s/t e/m)
                                            (mdo x <- get-state
                                                 (safe-div 2 x))
                                            0)))
(expect [2 1]
        (seq (t/from-right
              (s/run-state-t (s/t e/m)
                             (mdo x <- get-state
                                  (lift (safe-div 2 x)))
                             1))))

(expect "zero denominator!" (t/from-left
                             (run-monad (w/t e/m)
                                        (lift (safe-div 2 0)))))

(expect "zero denominator!" (t/from-left
                             (r/run-reader-t (r/t e/m)
                                             (lift (safe-div 2 0))
                                             0)))
(expect "zero denominator!" (t/from-left
                             (run-monad (e/t e/m)
                                        (lift (safe-div 2 0)))))

(expect "zero denominator!" (t/from-left
                             (r/run-reader-t (r/t e/m)
                                             (mdo x <- ask
                                                  (safe-div 2 x))
                                             0)))

(expect "zero denominator!" (t/from-left
                             (r/run-reader-t (r/t e/m)
                                             (s/run-state-t (s/t (r/t e/m))
                                                            (lift (lift (safe-div 2 0)))
                                                            3)
                                             4)))

(expect "zero denominator!" (t/from-left
                             (t/from-right
                              (run-monad (e/t e/m)
                                         (safe-div 2 0)))))

(expect nil (t/from-just (run-monad (m/t m/m)
                                    (safe-div 2 0))))

(expect nil (run-monad (m/t m/m) (lift (safe-div 2 0))))
