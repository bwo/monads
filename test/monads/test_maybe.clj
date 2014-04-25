(ns monads.test-maybe
  ;; blecchy side-effect of moving things around and being too lazy to
  ;; just import the relevant fns/entities
  (:use monads.core
        monads.maybe
        [monads.types :only [from-just]]
        monads.util
        [expectations :exclude [fail]])
  (:require [monads.identity :as i]))

(expect nil (run-monad maybe-m mzero))
(expect 3 (from-just (run-monad maybe-m (return 3))))
(expect nil (run-monad maybe-m (mdo x <- (return 3)
                              (guard (== x 2))
                              (return x))))
(expect 3 (from-just (run-mdo m
                              x <- (return 3)
                              (guard (== x 3))
                              (return x))))
(expect nil (run-monad maybe-m (mplus mzero mzero)))
(expect 3 (from-just (run-monad maybe-m (mplus mzero (return 3)))))
(expect 3 (from-just (run-monad maybe-m (mplus (return 3) (return 4)))))
(expect 3 (from-just (run-monad maybe-m (mplus (return 3) mzero))))


(expect nil (run-monad (maybe-t i/m) mzero))
(expect 3 (from-just (run-monad (maybe-t i/m) (return 3))))
(expect nil (run-monad (maybe-t i/m) (mdo x <- (return 3)
                              (guard (== x 2))
                              (return x))))
(expect 3 (from-just (run-mdo m
                              x <- (return 3)
                              (guard (== x 3))
                              (return x))))
(expect nil (run-monad (maybe-t i/m) (mplus mzero mzero)))
(expect 3 (from-just (run-monad (maybe-t i/m) (mplus mzero (return 3)))))
(expect 3 (from-just (run-monad (maybe-t i/m) (mplus (return 3) (return 4)))))
(expect 3 (from-just (run-monad (maybe-t i/m) (mplus (return 3) mzero))))

