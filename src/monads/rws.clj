(ns monads.rws
  (:require [monads.core :refer :all]
            [monads.types :as types :refer [fst snd thd]])
  (:use [monads.util :only [curryfn lazy-pair]]
        [babbage.monoid :only [<>]])
  (:import [monads.types Returned Triple]))

(defn run-rws-t [m computation state env]
  ((run-monad m computation) state env))

(defn rws-t [inner]
  (monad
   (mreturn [me x] (fn [s e] (types/mreturn inner (Triple. x s nil))))
   (bind [me m f] (fn [s e]
                    (run-mdo inner
                             ^Triple t <- (run-rws-t me m s e)
                             let a = (.f t)
                             s' = (.s t)
                             w = (.t t)
                             ^Triple t <- (run-rws-t me (f a) s' e)
                             (return (Triple. (.f t) (.s t) (<> w (.t t)))))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me c] (fn [s e]
                  (run-mdo inner
                           v <- c
                           (return (Triple. v s nil)))))
   types/MonadReader
   (ask [me] (fn [s e]
               (types/mreturn inner (Triple. e s nil))))
   (local [me f m] (fn [s e]
                     (run-rws-t me m s (f e))))
   types/MonadWriter
   (tell [me w] (fn [s e] (types/mreturn inner (Triple. nil s w))))
   (listen [me comp] (fn [s e]
                       (run-mdo inner
                                ^Triple t <- (run-rws-t me comp s e)
                                (return (Triple. [(.f t) (.t t)] (.s t) (.t t))))))
   (pass [me comp] (fn [s e]
                     (run-mdo inner
                              ^Triple t <- (run-rws-t me comp s e)
                              (return (Triple. (first (.f t))
                                               (.s t)
                                               ((second (.f t)) (.t t)))))))
   types/MonadState
   (get-state [me] (fn [s e] (types/mreturn inner (Triple. s s nil))))
   (put-state [me x] (fn [s e] (types/mreturn inner (Triple. x x nil))))
   (when (types/monaderror? inner)
     types/MonadError
     (throw-error [me err] (fn [s e] (run-rws-t me (lift (throw-error err)) s e)))
     (catch-error [me m h] (fn [s e] (run-monad inner (catch-error (run-rws-t me m s e)
                                                                  (fn [err] (run-rws-t me (h err) s e)))))))
   (when (types/monadfail? inner)
     types/MonadFail
     (fail [me msg] (fn [s e] (types/fail inner msg))))
   (when (types/monadplus? inner)
     types/MonadPlus
     (mzero [me] (fn [s e] (types/mzero inner)))
     (mplus [me lr] (fn [s e]
                      (types/mplus inner
                                   (lazy-pair
                                    (run-rws-t me (first lr) s e)
                                    (run-rws-t me (second lr) s e))))))))

(declare run-rws)

(defmonad rws-m
  (mreturn [me x] (fn [s e] (Triple. x s nil)))
  (bind [me m f]
        (fn [s e]
          (let [^Triple t (run-rws m s e)
                a (.f t)
                s' (.s t)
                w (.t t)
                ^Triple t (run-rws (f a) s' e)
                b (.f t)
                s'' (.s t)
                w' (.t t)]
            (Triple. b s'' (<> w w')))))

  types/MonadWriter
  (tell [me w] (fn [s e] (Triple. nil s w)))
  (listen [me comp] (fn [s e] (let [^Triple t (run-rws comp s e)]
                               (Triple. [(fst t) (thd t)]
                                        (snd t)
                                        (thd t)))))
  (pass [me comp] (fn [s e] (let [^Triple t (run-rws comp s e)]
                             (Triple. (first (fst t))
                                      (snd t)
                                      ((second (fst t)) (thd t))))))

  types/MonadReader
  (ask [me] (fn [s e] (Triple. e s nil)))
  (local [me f m] (fn [s e] (run-rws m s (f e))))

  types/MonadState
  (get-state [me] (fn [s e] (Triple. s s nil)))
  (put-state [me x] (fn [s e] (Triple. x x nil))))

(defn run-rws [computation state env]
  ((run-monad rws-m computation) state env))

(defn lift-catch [m h]
  (Returned.
   (fn [t]
     (fn [s e]
       (run-monad (types/inner t)
                  (catch-error (run-rws-t t m s e)
                               (fn [err] (run-rws-t t (h err) s e))))))))

(def t rws-t)
(def m rws-m)
