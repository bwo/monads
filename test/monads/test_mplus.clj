(ns monads.test-mplus
  (:require [monads.state :as s]
            [monads.list :as l]
            [monads.error :as e]
            [monads.maybe :as m]
            [monads.reader :as r]
            [monads.types :as t]
            [monads.util :as u]
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

(expect [[3 4 5]
         [5 12 13]
         [6 8 10]]
        (take 3 (run-monad l/list-m
                           (mdo a <- (range 1 200)
                                b <- (range (inc a) 200)
                                let ab = (+ (* a a) (* b b))
                                c <- (range 1 200)
                                (u/guard (== (* c c) ab))
                                (return (list a b c))))))

(expect [[3 4 5]
         [5 12 13]
         [6 8 10]]
        (map t/from-just (take 3 (run-monad (m/maybe-t l/list-m)
                                            (mdo a <- (map t/just (range 1 200))
                                                 b <- (map t/just (range (inc a) 200))
                                                 let ab = (+ (* a a) (* b b))
                                                 c <- (map t/just (range 1 200))
                                                 (u/guard (== (* c c) ab))
                                                 (return (list a b c)))))))

(expect [5 6] (run-monad l/list-m (mplus (return 5) (return 6))))

(expect [[5 ["hi"]] [6 ["hi"]]]
        (map seq (run-monad (w/t l/m) (mdo
                                       (tell ["hi"])
                                       (mplus (return 5) (return 6))))))

(expect [[5 ["hi"]] [3 ["hi"]]]
        (map seq (run-monad (w/t l/m) (mdo
                                       (tell ["hi"])
                                       x <- (mplus (return 5) (return 6))
                                       (u/guard (< x 6))
                                       (mplus (return x) (return 3))))))

(expect nil
        (run-monad (w/t m/m) (mdo
                              (tell ["hi"])
                              x <- (mplus (return 5) (return 6))
                              (u/guard (> x 5))
                              (mplus (return x) (return 3)))))

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
