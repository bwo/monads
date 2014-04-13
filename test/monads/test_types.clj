(ns monads.test-types
  (:require [monads.types :refer :all :exclude [fail]])
  (:use [expectations :exclude [nothing]])
  (:import [monads.types Pair Triple]))

(expect 1 (fst (Pair. 1 2)))
(expect 2 (snd (Pair. 1 2)))
(expect 1 (fst (Triple. 1 2 3)))
(expect 2 (snd (Triple. 1 2 3)))
(expect 3 (thd (Triple. 1 2 3)))

(expect Exception (from-just nothing))
(expect nothing (from-just (just nothing)))

(expect 1 (maybe 1 inc nothing))
(expect 2 (maybe 1 inc (just 1)))

(expect Exception (from-right (left 1)))
(expect Exception (from-left (right 1)))
(expect 1 (from-right (right 1)))
(expect 1 (from-left (left 1)))
(expect 2 (either inc dec (left 1)))
(expect 0 (either inc dec (right 1)))
