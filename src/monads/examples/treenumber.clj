(ns monads.examples.treenumber
  (:require [monads.core :refer :all]
            [monads.state :refer :all]))

;; tree-numbering.
;; Our trees: {:val int :left tree :right tree}, or nil
(defn node [v left right]
  {:val v :left left :right right})
(defn index-in-list [p lst]
  (second (first (filter (comp p first) (map vector lst (range))))))
(defn n-node [x table]
  (if-let [i (get table x)]
    [table i]
    (let [c (count table)]
      [(assoc table x c) c])))
(defn number-node [x]
  (mdo table <- get-state
       let [newtable newpos] = (n-node x table)
       (put-state newtable)
       (return newpos)))
(defn number-tree [{:keys [val left right] :as tree}]
  (if-not tree
    (return nil)
    (mdo num <- (number-node val)
         nt1 <- (number-tree left)
         nt2 <- (number-tree right)
         (return (node num nt1 nt2)))))
(defn num-tree [t]
  (eval-state (number-tree t) {}))

(def a-tree (node "a"
                  (node "a" nil (node "c"
                                      (node "b" nil (node "a" nil nil))
                                      (node "c"
                                            nil
                                            (node "d" nil nil))))
                  (node "d"
                        (node "c" nil nil)
                        (node "b"
                              (node "e"
                                    (node "a" nil nil)
                                    nil)))))

(defn bintree [n]
  (reduce #(node %2 %1 %1) nil (range n)))
