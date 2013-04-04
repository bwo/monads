(ns monads.test-writer
  (:require [monads.writer :as w]
            [monads.error :as e]
            [monads.types :as t])
  (:use monads.core
        expectations))

(expect [[1 [2]] [3 2]]
        (seq (run-monad w/writer-m (mdo x <- (return 3)
                                        (tell [3])
                                        y <- (listen (mdo
                                                      (tell [2])
                                                      (return 1)))
                                        (return y)))))

(expect [[1 ()] ["3"]]
        (seq (run-monad w/writer-m
                        (listens rest (>> (tell ["3"]) (return 1))))))

(expect [[1 [2]] [3 2]]
        (seq (t/from-right (run-monad (w/t e/m) (mdo x <- (return 3)
                                                      (tell [3])
                                                      y <- (listen (mdo
                                                                    (tell [2])
                                                                    (return 1)))
                                                      (return y))))))
(expect [1 [3 2]]
        (seq (run-monad w/writer-m (mdo x <- (return 3)
                                        (tell [3])
                                        y <- (pass (mdo
                                                      (tell [1])
                                                      (tell [2])
                                                      (return [1 rest])))
                                        (return y)))))

(expect [1 [3 2]]
        (seq (t/from-right (run-monad (w/t e/m) (mdo x <- (return 3)
                                                     (tell [3])
                                                     y <- (pass (mdo
                                                                 (tell [1])
                                                                 (tell [2])
                                                                 (return [1 rest])))
                                                     (return y))))))

(expect [1 [3 2]]
        (seq (run-monad w/writer-m (mdo x <- (return 3)
                                        (tell [3])
                                        y <- (censor rest(mdo
                                                          (tell [1])
                                                          (tell [2])
                                                          (return 1)))
                                        (return y)))))

(expect [1 [3 2]]
        (seq (t/from-right (run-monad (w/t e/m) (mdo x <- (return 3)
                                                     (tell [3])
                                                     y <- (censor rest(mdo
                                                                       (tell [1])
                                                                       (tell [2])
                                                                       (return 1)))
                                                     (return y))))))
