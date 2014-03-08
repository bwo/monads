(ns monads.test-state
  (:require [monads.core :refer :all]
            [monads.types :as t :refer [fst snd]]
            [monads.state :as s]
            [monads.maybe :as m]
            [monads.util :as u]
            [expectations :refer [expect]]))

(defn when-state [pred action]
  (mdo st <- get-state
       (u/mwhen (pred st)
                action)))

(def simple (mdo (when-state #(> % 10)
                             (modify * 2))
                 (return 1)))

(def simple-zero (mdo st <- get-state
                      (u/guard (> st 10))
                      (modify * 2)
                      (return 1)))

(expect (= (t/just 1)
           (s/eval-state-t (s/t m/m) simple-zero 30)))
(expect (= (t/just 50)
           (s/exec-state-t (s/t m/m) simple-zero 25)))
(expect nil
        (s/eval-state-t (s/t m/m) simple-zero 1))
(expect nil
        (s/exec-state-t (s/t m/m) simple-zero 1))

(expect (= (t/->Pair 1 60)
           (s/run-state simple 30)))
(expect (= (t/->Pair 1 3)
           (s/run-state simple 3)))

