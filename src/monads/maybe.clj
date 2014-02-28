(ns monads.maybe
  (:require [monads.core :refer :all]
            [monads.types :as types]
            [monads.util :as u])
  (:use [monads.types :only [from-just nothing? just nothing maybe]]))

(defn maybe-t [inner]
  (monad
   (mreturn [me v] (types/mreturn inner (just v)))
   (bind [me m f] (run-mdo inner
                           v <- m
                           (if (nothing? v)
                             (return nothing)
                             (run-monad me (f (from-just v))))))
   types/MonadFail
   (fail [me _] (types/mreturn inner nothing))
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (run-monad inner (lift-m just m)))
   types/MonadPlus
   (mzero [me] (types/mreturn inner nothing))
   (mplus [me lr]
          (run-mdo inner
                   lv <- (run-monad me (first lr))
                   (if lv
                     (return lv)
                     (run-monad me (second lr)))))
   (when (types/monaderror? inner)
     types/MonadError
     (throw-error [me o] (run-monad me (lift (throw-error o))))
     (catch-error [me m h] (run-monad inner
                                      (catch-error (run-monad me m)
                                                   (fn [e] (run-monad me (h e)))))))
   (when (types/monadstate? inner)
     types/MonadState
     (get-state [me] (run-monad me (lift get-state)))
     (put-state [me s] (run-monad me (lift (put-state s)))))
   (when (types/monadreader? inner)
     types/MonadReader
     (ask [me] (run-monad me (lift ask)))
     (local [me f m] (run-monad inner (local f (run-monad me m)))))
   (when (types/monadwriter? inner)
     types/MonadWriter
     (tell [me w] (run-monad me (lift (tell w))))
     (pass [me m] (run-monad me (lift (pass m))))
     (listen [me m] (run-monad me (lift (listen m)))))))

(defmonad maybe-m
  (mreturn [me v] (just v))
  (bind [me m f]
        (when m (run-monad maybe-m (f (from-just m)))))
  types/MonadFail
  (fail [me _] nothing)
  types/MonadPlus
  (mzero [me] nothing)
  (mplus [me lr]
         (let [lv (run-monad maybe-m (first lr))]
           (or lv
               (run-monad maybe-m (second lr))))))

(def m maybe-m)
(def t maybe-t)

