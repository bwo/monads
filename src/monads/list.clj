(ns monads.list
  (:require [monads.core :refer :all]
            [monads.types :as types]
            [monads.util :as u]))

(defmonad list-m
  (mreturn [me v] [v])
  (bind [me m f] (u/mcat (comp (partial run-monad me) f) m))
  types/MonadFail
  (fail [me msg] nil)
  types/MonadPlus
  (mzero [me] ())
  (mplus [me lr] (concat (run-monad me (first lr))
                         (run-monad me (second lr)))))

(def m list-m)
