(ns monads.applicative
  (:require [monads.types :as types]
            [monads.util :as u]
            [monads.core :as core])
  (:import [monads.types Just Either Mplus Returned Bind Return]))

(set! *warn-on-reflection* true)

(deftype Pure [f]
  Object
  (toString [this]
    (with-out-str (print f))))

(def pure ->Pure)

(defn cpure [arity f]
  (pure (u/ecurry arity f)))

(defprotocol Applicative
  (fapply [me f]))

(defn <*>
  [f & as]
  (reduce (fn [acc a] (fapply a acc)) f as))

(defn pure->mon [o]
  (types/if-instance Pure o
    (Return. (.f o))
    o))

(defn fapply-mon [v f]
  (u/ap (pure->mon f) v))

(extend-protocol Applicative
  Just
  (fapply [me o]    
    (when-let [f (types/cond-instance o
                     Just (.v o)
                     Pure (.f o))]
      (types/just (f (.v me)))))

  Either
  (fapply [me o]
    (if (types/right? me)
      (if-let [f (types/cond-instance o
                       Pure (.f o)
                       Either (types/either (constantly nil) identity o))]
        (types/right (f (types/from-right me)))
        o)
      me))

  Return
  (fapply [me f] (fapply-mon me f))

  Bind
  (fapply [me f] (fapply-mon me f)))
