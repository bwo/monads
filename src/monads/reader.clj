(ns monads.reader
  (:require [monads.core :refer :all]
            [monads.types :as types])
  (:use [monads.util :only [curryfn lazy-pair]])
  (:import [monads.types Returned]))

(defn run-reader-t [m comp e]
  ((run-monad m comp) e))

(defn reader-t [inner]
  (monad
   (mreturn [me v] (constantly (types/mreturn inner v)))
   (bind [me m f] (fn [e]
                    (run-mdo inner
                             a <- (m e)
                             (run-reader-t me (f a) e))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me c] (fn [e] (run-monad inner c)))
   types/MonadReader
   (ask [me] (fn [e] (types/mreturn inner e)))
   (local [me f m] (fn [e] (run-reader-t me m (f e))))
   (when (types/monadwriter? inner)
     types/MonadWriter
     (tell [me w] (fn [e] (run-reader-t me (lift (tell w)) e)))
     (pass [me m] (fn [e] (run-reader-t me (lift (pass m)) e)))
     (listen [me m] (fn [e] (run-reader-t me (lift (listen m)) e))))
   (when (types/monadstate? inner)
     types/MonadState
     (get-state [me] (fn [e] (run-reader-t me (lift get-state) e)))
     (put-state [me s] (fn [e] (run-reader-t me (lift (put-state s)) e))))
   (when (types/monaderror? inner)
     types/MonadError
     (throw-error [me o] (fn [e] (run-reader-t me (lift (throw-error o)) e)))
     (catch-error [me m h]
                  (fn [e] (run-monad inner
                                    (catch-error (run-reader-t me m e)
                                                 (fn [err] (run-reader-t me (h err) e)))))))
   (when (types/monadfail? inner)
     types/MonadFail
     (fail [me msg] (fn [e] (types/fail inner msg))))
   (when (types/monadplus? inner)
     types/MonadPlus
     (mzero [me] (constantly (types/mzero inner)))
     (mplus [me lr] (fn [e]
                      (types/mplus inner
                                   (lazy-pair (run-reader-t me (first lr) e)
                                              (run-reader-t me (second lr) e))))))))

(declare run-reader)

(defmonad reader-m
  (mreturn [me x] (constantly x))
  (bind [me m f]
        (fn [e] (run-reader (f (run-reader m e)) e)))
  types/MonadReader
  (ask [me] identity)
  (local [me f m] (comp (partial run-reader m) f)))

(defn run-reader [comp e]
  ((run-monad reader-m comp) e))

(def t reader-t)
(def m reader-m)
