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
