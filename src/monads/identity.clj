(ns monads.identity
  (:use [monads.core :only [defmonad run-monad]]))

(defmonad identity-m
  (mreturn [me x] x)
  (bind [me m f] (run-monad me (f m))))

(def m identity-m)
