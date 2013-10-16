(ns monads.writer
  (:require [monads.core :refer :all]
            [monads.types :as types :refer [fst snd]]
            [monads.util :as u])
  (:use [monads.util :only [lazy-pair]]
        [babbage.monoid :only [<>]])
  (:import [monads.types Returned Pair]))

(defn writer-t [inner]
  (monad
   (mreturn [me v] (types/mreturn inner (Pair. v nil)))
   (bind [me m f]
         (run-mdo inner
                  ^Pair p <- (run-monad me m)
                  let a = (.fst p) w = (.snd p)
                  ^Pair p <- (run-monad me (f a))
                  let b = (.fst p) w' = (.snd p)
                  (return (Pair. b (<> w w')))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me c] (run-mdo inner
                         a <- c
                         (return (Pair. a nil))))
   (when (types/monadplus? inner)
     types/MonadPlus
     (mzero [me] (types/mzero inner))
     (mplus [me lr] (types/mplus inner
                                 (lazy-pair
                                  (run-monad me (first lr))
                                  (run-monad me (second lr))))))
   (when (types/monadfail? inner)
     types/MonadFail
     (fail [me msg] (types/fail inner msg)))
   (when (types/monadstate? inner)
     types/MonadState
     (get-state [me] (run-monad me (lift get-state)))
     (put-state [me o] (run-monad me (lift (put-state o)))))
   (when (types/monadreader? inner)
     types/MonadReader
     (ask [me] (run-monad me (lift ask)))
     (local [me f m] (run-monad inner (local f (run-monad me m)))))
   (when (types/monaderror? inner)
     types/MonadError
     (throw-error [me e] (run-monad me (lift (throw-error e))))
     (catch-error [me m h] (run-monad me (catch-error (run-monad me m)
                                                      (fn [e] (run-monad me (h m)))))))
   types/MonadWriter
   (tell [me w] (types/mreturn inner (Pair. nil w)))
   (listen [me c] (run-mdo inner
                           ^Pair p <- (run-monad me c)
                           (return (Pair. [(.fst p) (.snd p)] (.snd p)))))
   (pass [me c] (run-mdo inner
                         ^Pair p <- (run-monad me c)
                         (return (Pair. (first (.fst p))
                                        ((second (.fst p)) (.snd p))))))))

(defmonad writer-m
  (mreturn [me v] (Pair. v nil))
  (bind [me m f]
        (let [^Pair p (run-monad me m)
              a (.fst p)
              w (.snd p)
              ^Pair p (run-monad me (f a))
              b (.fst p)
              w' (.snd p)]
          (Pair. b (<> w w'))))
  types/MonadWriter
  (tell [me w] (Pair. nil w))
  (listen [me comp] (let [^Pair p (run-monad me comp)]
                      (Pair. [(.fst p) (.snd p)] (.snd p))))
  (pass [me comp] (let [^Pair p (run-monad me comp)]
                    (Pair. (first (.fst p))
                           ((second (.fst p)) (.snd p))))))


(def t writer-t)
(def m writer-m)

(def lift-local u/lift-local)
(def lift-catch u/lift-catch)

