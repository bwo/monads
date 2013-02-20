(ns monads.identity
  (:use [monads.core :only [defmonad]]))

(defmonad identity-m
  :bind (fn [m f] (f m))
  :return identity)
