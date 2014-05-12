(ns monads.error
  (:require [monads.core :refer :all]
            [monads.types :as types]
            [monads.util :as u])
  (:use [monads.types :only [right left left? either]]))

(defn error-t [inner]
  (monad
   (mreturn [me v] (types/mreturn inner (right v)))
   (bind [me m f] (run-mdo inner
                           x <- (run-monad me m)
                           (either #(types/mreturn inner (left %))
                                   #(run-monad me (f %))
                                   x)))
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (run-monad inner (>>= m (fn [x] (types/mreturn inner (right x))))))
   types/MonadFail
   (fail [me msg] (types/mreturn inner (left msg)))
   types/MonadPlus
   (mzero [me] (types/mreturn inner (left nil)))
   (mplus [me lr] (run-mdo inner
                           l <- (run-monad me (first lr))
                           (if (left? l)
                             (run-monad me (second lr))
                             (return l))))
   (when (types/monadstate? inner)
     types/MonadState
     (get-state [me] (run-monad me (lift get-state)))
     (put-state [me s] (run-monad me (lift (put-state s)))))
   (when (types/monadreader? inner)
     types/MonadReader
     (ask [me] (run-monad me (lift ask)))
     (local [me f m] (run-monad inner
                                (local f (run-monad me m)))))
   (when (types/monadwriter? inner)
     types/MonadWriter
     (tell [me o] (run-monad me (lift (tell o))))
     (listen [me m] (run-monad me (lift (listen m))))
     (pass [me m] (run-monad me (lift (pass m)))))
   types/MonadError
   (throw-error [me err] (types/mreturn inner (left err)))
   (catch-error [me comp handler]
                (run-mdo inner
                         v <- (run-monad me comp)
                         (either #(run-monad me (handler %))
                                 #(types/mreturn inner (right %)) v)))))

(let [mzero (left nil)]
  (defmonad error-m
    (mreturn [me v] (right v))
    (bind [me m f] 
          (either left #(run-monad me (f %)) (run-monad me m)))
    types/MonadFail
    (fail [me msg] (left msg))
    types/MonadPlus
    (mzero [me] mzero)
    (mplus [me lr] (let [v (run-monad me (first lr))]
                     (if (left? v)
                       (run-monad me (second lr))
                       v)))
    types/MonadError
    (throw-error [me e] (left e))
    (catch-error [me comp handler]
                 (let [v (run-monad me comp)]
                   (either #(run-monad me (handler %)) right v)))))

(def m error-m)
(def t error-t)
