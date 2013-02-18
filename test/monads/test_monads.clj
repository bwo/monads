(ns monads.test-monads
  (:use monads.core
        monads.monads
        expectations))

(given [m] (do (expect nil (run-monad m mzero))
               (expect 3 (from-just (run-monad m (return 3))))
               (expect nil (run-monad m (mdo x <- (return 3)
                                             (guard (== x 2))
                                             (return x))))
               (expect 3 (from-just (run-mdo m
                                             x <- (return 3)
                                             (guard (== x 3))
                                             (return x))))
               (expect nil (run-monad m (mplus mzero mzero)))
               (expect 3 (from-just (run-monad m (mplus mzero (return 3)))))
               (expect 3 (from-just (run-monad m (mplus (return 3) (return 4)))))
               (expect 3 (from-just (run-monad m (mplus (return 3) mzero)))))
       maybe-m
       (maybe-t identity-m))




