(ns monads.test-error
  (:require [monads.error :as e]
            [monads.maybe :as m]
            [monads.reader :as r]
            [monads.state :as s]
            [monads.types :as t]
            [monads.util :as u]
            [monads.writer :as w])
  (:use [expectations :exclude [fail]]
        monads.core))

(expect -1
        (->> (catch-error
              (mdo x <- (return 0)
                   (u/mwhen (== x 0)
                            (throw-error "x is zero"))
                   (return (/ 4 x)))
              (constantly (return -1)))
             (run-monad e/m)
             t/from-right))

(expect -1 (->> (r/lift-catch
                 (mdo x <- ask
                      (u/mwhen (== x 0)
                               (lift (throw-error "x is zero")))
                      (return (/ 4 x)))
                 (constantly (return -1)))
                (#(r/run-reader-t (r/t e/m) % 0))
                t/from-right))

(expect [-1 ["handled"]]
        (->> (w/lift-catch
              (mdo x <- (return 0)
                   (u/mwhen (== x 0)
                            (lift (throw-error "x is zero")))
                   (tell ["made it"])
                   (return (/ 4 x)))
              (fn [err] (>> (tell ["handled"]) (return -1))))
             (run-monad (w/t e/m))
             t/from-right
             seq))

(expect [2 ["made it"]]
        (->> (w/lift-catch
              (mdo x <- (return 2)
                   (u/mwhen (== x 0)
                            (lift (throw-error "x is zero")))
                   (tell ["made it"])
                   (return (/ 4 x)))
              (fn [err] (>> (tell ["handled"]) (return -1))))
             (run-monad (w/t e/m))
             t/from-right
             seq))

(expect [-1 0]
        (->> (s/lift-catch
              (mdo x <- get-state
                   (u/mwhen (== x 0)
                            (lift (throw-error "x is zero")))
                   (put-state 4)
                   (return (/ 4 x)))
              (constantly (return -1)))
             (#(s/run-state-t (s/t e/m) % 0))
             t/from-right
             seq))


(expect -1
        (->> (m/lift-catch
              (mdo x <- (return 0)
                   (u/mwhen (== x 0)
                            (lift (throw-error "x is zero")))
                   mzero)
              (constantly (return -1)))
             (run-monad (m/t e/m))
             t/from-right
             t/from-just))

(expect t/nothing?
        (->> (m/lift-catch
              (mdo x <- (return 1)
                   (u/mwhen (== x 0)
                            (lift (throw-error "x is zero")))
                   mzero)
              (constantly (return -1)))
             (run-monad (m/t e/m))
             t/from-right))

(expect ["3" nil]
        (->> (run-monad (w/t (m/t e/m)) (lift (m/lift-catch (lift (throw-error "3")) return)))
             t/from-right
             t/from-just
             seq))
