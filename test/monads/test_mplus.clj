(ns monads.test-mplus
  (:require [monads.state :as s]
            [monads.list :as l]
            [monads.error :as e]
            [monads.maybe :as m]
            [monads.reader :as r]
            [monads.types :as t]
            [monads.writer :as w])
    (:use expectations
          monads.core))

(expect 3 (->> (mplus (return 3) (return 6))
               (run-monad (e/error-t m/maybe-m))
               t/from-just
               t/from-right))

(expect 6  (->> (mplus mzero (return 6))
               (run-monad (e/error-t m/maybe-m))
               t/from-just
               t/from-right))

(expect 6  (->> (mplus mzero (return 6))
               (run-monad e/error-m)
               t/from-right))

(expect 3  (->> (mplus (return 3) mzero)
               (run-monad e/error-m)
               t/from-right))

(expect [5 6] (run-monad l/list-m (mplus (return 5) (return 6))))

(expect [5 6] (r/run-reader-t (r/reader-t l/list-m) (mplus ask (asks inc)) 5))

(expect 5 (t/from-just (r/run-reader-t (r/reader-t m/maybe-m) (mplus ask (asks inc)) 5)))

(expect 5 (t/from-just (r/run-reader (run-monad (m/maybe-t r/reader-m) (mplus (lift (asks inc))
                                                                              mzero)) 4)))

(expect 5 (t/from-just (r/run-reader (run-monad (m/maybe-t r/reader-m)
                                                (mplus mzero
                                                       (lift (asks inc)))) 4)))

(expect [5 6] (r/run-reader-t
               (r/t l/m) (s/eval-state-t (s/t (r/t l/m))
                                         (mplus (modify dec)
                                                (lift (local inc
                                                             (mdo x <- ask
                                                                  (return (+ x 2))))))
                                         6) 3))

(expect 6 (t/from-just (r/run-reader-t (r/reader-t m/maybe-m) (mplus mzero (asks inc)) 5)))
