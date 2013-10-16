(ns monads.test-rws
  (:require [monads.core :refer :all :exclude [fail]]
            [monads.error :as e]
            [monads.rws :as r]
            [monads.types :as t])
  (:use [expectations :only [expect]]))


(expect [[1 [2]] :state [3 2]]
        (seq (r/run-rws (mdo x <- (return 3)
                             (tell [3])
                             y <- (listen (mdo
                                           (tell [2])
                                           (return 1)))
                             (return y))
                        :state
                        :env)))

(expect [[1 ()] :state ["3"]]
        (seq (r/run-rws (listens rest (>> (tell ["3"]) (return 1))) :state :env)))

(expect [[1 [2]] :state [3 2]]
        (seq (t/from-right (r/run-rws-t (r/t e/m) (mdo x <- (return 3)
                                                       (tell [3])
                                                       y <- (listen (mdo
                                                                     (tell [2])
                                                                     (return 1)))
                                                       (return y))
                                        :state :env))))
(expect [1 :state [3 2]]
        (seq (r/run-rws (mdo x <- (return 3)
                             (tell [3])
                             y <- (pass (mdo
                                         (tell [1])
                                         (tell [2])
                                         (return [1 rest])))
                             (return y))
                        :state :env)))

(expect [1 :state [3 2]]
        (seq (t/from-right (r/run-rws-t (r/t e/m) (mdo x <- (return 3)
                                                     (tell [3])
                                                     y <- (pass (mdo
                                                                 (tell [1])
                                                                 (tell [2])
                                                                 (return [1 rest])))
                                                     (return y))
                                      :state :env))))

(expect [1 :etats [3 2]]
        (seq (r/run-rws (mdo x <- (return 3)
                             (tell [3])
                             (put-state :etats)
                             y <- (censor rest (mdo
                                                (tell [1])
                                                (tell [2])
                                                (return 1)))
                             (return y))
                        :state
                        :env)))

(expect [1 "etats" [3 2]]
        (seq (t/from-right (r/run-rws-t (r/t e/m) (mdo x <- (return 3)
                                                       (tell [3])
                                                       f <- ask
                                                       (modify (comp f reverse name))
                                                       y <- (censor rest (mdo
                                                                          (tell [1])
                                                                          (tell [2])
                                                                          (return 1)))
                                                       (return y))
                                        :state #(apply str %)))))
