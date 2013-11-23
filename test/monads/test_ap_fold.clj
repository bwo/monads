(ns monads.test-ap-fold
  (:require [monads.applicative :as app]
            [monads.examples.applicative-fold :as f])
  (:use [expectations]))


(expect 3 (f/fold f/avg [1 2 3 4 5]))

(expect [15 5 3]
        (f/fold (app/<*> (app/cpure 3 vector) f/sum f/count f/avg) [1 2 3 4 5]))
