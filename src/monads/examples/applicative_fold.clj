(ns monads.examples.applicative-fold
  (:refer-clojure :exclude [count])
  (:require [monads.applicative :as app]
            [monads.types :as types]

            [clojure.algo.generic.functor :as f])
  (:import [monads.applicative Pure]
           [monads.types Pair]))

(deftype Fold [combine start finish]
  app/Applicative
  (fapply [me o]
    (let [[combinel startl finishl] (types/cond-instance o
                                        Pure [(fn [_ _] nil) nil (fn [_] (.f o))]
                                        Fold [(.combine o) (.start o) (.finish o)])
          [combiner startr finishr] [(.combine me) (.start me) (.finish me)]]
      (Fold. (fn [^Pair p a] (Pair. (combinel (.fst p) a) (combiner (.snd p) a)))
             (Pair. startl startr)
             (fn [^Pair p] ((finishl (.fst p)) (finishr (.snd p))))))))

(defmethod f/fmap Fold
  [f ^Fold fold]
  (Fold. (.combine fold) (.start fold) (comp f (.finish fold))))

(defn fold [^Fold f as]
  ((.finish f) (reduce (.combine f) (.start f) as)))

(def sum (Fold. + 0 identity))
(def count (Fold. (fn [a _] (inc a)) 0 identity))
(def avg (app/<*> (app/cpure 2 /) sum count))

(comment
  (= 3 (fold avg [1 2 3 4 5]))
  (= [15 5 3] (fold (app/<*> (app/cpure 3 vector) sum count avg) [1 2 3 4 5])))
