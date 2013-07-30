(ns monads.state
  (:require [monads.core :refer :all :exclude [get-state put-state modify]]
            [monads.types :as types :refer [fst snd]])
  (:use [monads.util :only [curryfn lazy-pair]])
  (:import [monads.types Returned Pair]))

(defn run-state-t [m computation initial-state]
  ((run-monad m computation) initial-state))

(defn state-t [inner]
  (monad
   (mreturn [me x] (fn [s] (types/mreturn inner (Pair. x s))))
   (bind [me m f] (fn [s]
                    (run-mdo inner
                             ^Pair p <- (m s)
                             let v = (.fst p) s = (.snd p)
                             (run-state-t me (f v) s))))
   
   types/MonadTrans
   (inner [me] inner)
   (lift [me m] (fn [s]
                  (run-mdo inner
                           v <- m
                           (return (Pair. v s)))))
   
   types/MonadState
   (get-state [me] (fn [s] (types/mreturn inner (Pair. s s))))
   (put-state [me v] (fn [_] (types/mreturn inner (Pair. v v))))

   (when (types/monadreader? inner)
     types/MonadReader
     (ask [me] (fn [s] (run-state-t me (lift ask) s)))
     (local [me f m] (fn [s] (run-monad inner
                                       (local f (run-state-t me m s))))))

   (when (types/monadwriter? inner)
     types/MonadWriter
     (tell [me w] (fn [s] (run-state-t me (lift (tell w)) s)))
     (pass [me m] (fn [s] (run-state-t me (lift (pass m)) s)))
     (listen [me m] (fn [s] (run-state-t me (lift (listen m)) s))))

   (when (types/monaderror? inner)
     types/MonadError
     (throw-error [me e] (fn [s] (run-state-t me (lift (throw-error e)) s)))
     (catch-error [me m h]
                  (fn [s] (run-monad inner
                                    (catch-error (run-state-t me m s)
                                                 (fn [e] (run-state-t me (h e) s)))))))
   
   (when (types/monadfail? inner)
     types/MonadFail
     (fail [me msg] (fn [_] (types/fail inner msg))))
   
   (when (types/monadplus? inner)
     types/MonadPlus
     (mzero [me] (fn [_] (types/mzero inner)))
     (mplus [me lr]
            (fn [s]
              (types/mplus inner
                           (lazy-pair
                            (run-state-t me (first lr) s)
                            (run-state-t me (second lr) s))))))))

(declare run-state)

(defmonad state-m
  (mreturn [me x] (fn [s] (Pair. x s)))
  (bind [me m f]
        (fn [s]
          (let [^Pair p (m s)]
            (run-state (f (.fst p)) (.snd p)))))
  types/MonadState
  (get-state [me] (fn [s] (Pair. s s)))
  (put-state [me v] (fn [_] (Pair. v v))))

(defn run-state [computation initial-state]
  ((run-monad state-m computation) initial-state))

(def eval-state (comp fst run-state))
(def exec-state (comp snd run-state))
(defn exec-state-t [m comp initial-state]
  (run-monad (types/inner m) (lift-m snd (run-state-t m comp initial-state))))
(defn eval-state-t [m comp initial-state]
  (run-monad (types/inner m) (lift-m fst (run-state-t m comp initial-state))))

(def t state-t)
(def m state-m)

(defn lift-local [f m]
  (Returned.
   (curryfn [t s]
     (run-monad (types/inner t)
                (local f (run-state-t t m s))))))

(defn lift-catch [m h]
  (Returned.
   (curryfn [t s]
     (run-monad (types/inner t)
                (catch-error (run-state-t t m s)
                             (fn [e] (run-state-t t (h e) s)))))))
