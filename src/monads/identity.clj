(ns monads.identity
  (:use [monads.core :only [defmonad run-monad]]))

(defmonad identity-m
  :bind (fn [m f] (run-monad identity-m (f m)))
  :return identity)

(def m identity-m)
