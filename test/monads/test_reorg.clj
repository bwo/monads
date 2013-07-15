(ns monads.test-reorg
  (:require [monads.maybe :as m]
            [monads.cont :as c]
            [monads.types :as t]
            [monads.util :as u])
  (:use monads.core
        [expectations :exclude [fail]]))

;; actually just testing the stackoverflow-avoiding code

(expect nil (->> (repeat 10000 mzero)
                 (reduce mplus mzero)
                 c/reorganize
                 (run-monad (m/t c/m))
                 c/run-cont))

(expect 10000 (->> (range 10000)
                   (reduce (fn [acc _] (>>= acc (comp return inc))) (return 0))
                   c/reorganize
                   (run-monad (m/t c/m))
                   c/run-cont
                   t/from-just))

(expect 40000
        (t/from-just
         (c/run-cont
          (run-monad (m/t c/m)
                     (-> (reduce (fn [acc _] (>>= (mplus mzero acc) (comp return inc)))
                                 (mplus (reduce mplus mzero (repeat 40000 mzero)) (return 0))
                                 (range 40000))
                         c/reorganize)))))
