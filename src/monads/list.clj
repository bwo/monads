(ns monads.list
  (:require [monads.core :refer :all]
            [monads.types :as types]
            [monads.util :as u]))


(defn mcat [f xs]
  (lazy-seq
   (if (not (seq xs))
     nil
     (concat (f (first xs)) (mcat f (rest xs))))))

(defmonad list-m
  (mreturn [me v] [v])
  (bind [me m f] (mcat (comp (partial run-monad me) f) m))
  types/MonadFail
  (fail [me msg] nil)
  types/MonadPlus
  (mzero [me] ())
  (mplus [me lr] (concat (run-monad me (first lr))
                         (run-monad me (second lr)))))

;; note that this is not always a monad.
(defn list-t [inner]
  (monad
   (mreturn [me v] (types/mreturn inner [v]))
   (bind [me m f] (run-mdo inner
                           a <- (run-monad me m)
                           b <- (u/sequence-m (map (comp (partial run-monad me) f) a))
                           (return (apply concat b))))
   types/MonadPlus
   (mzero [me] (types/mreturn inner ()))
   (mplus [me lr] (run-mdo inner
                           (u/lift-m-2 concat
                                       (run-monad me (first lr))
                                       (run-monad me (second lr)))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (run-monad inner (>>= m (comp return list))))))


(def m list-m)
(def t list-t)
