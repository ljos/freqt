(ns freqt.t-core
  (:refer-clojure :exclude [flatten])
  (:require [midje.sweet :refer [fact facts truthy falsey]]
            [freqt.core :refer :all])
  (:import [freqt.core OrderedTree]))

(def t1
  (OrderedTree.
   0
   [{:depth 0           :label :r :first-child 1                }
    {:depth 1 :parent 0 :label :a :first-child 2 :next-sibling 6}
    {:depth 2 :parent 1 :label :a                :next-sibling 3}
    {:depth 2 :parent 1 :label :b                :next-sibling 4}
    {:depth 2 :parent 1 :label :a                :next-sibling 5}
    {:depth 2 :parent 1 :label :b                               }
    {:depth 1 :parent 0 :label :a :first-child 7                }
    {:depth 2 :parent 6 :label :a                :next-sibling 8}
    {:depth 2 :parent 6 :label :a                :next-sibling 9}
    {:depth 2 :parent 6 :label :b                               }]))

(def start-a (OrderedTree. 0 [{:depth 0 :label :a}]))

(facts "Building trees."
  (fact "Building a tree from empty should be the same as setting it up by hand."
    (-> (OrderedTree. 0 [{:depth 0 :label :r}])
        (assoc-child (make-tree :a))
        (assoc-child (make-tree :a))
        (assoc-sibling (make-tree :b))
        (assoc-sibling (make-tree :a))
        (assoc-sibling (make-tree :b))
        parent
        (assoc-sibling (make-tree :a))
        (assoc-child (make-tree :a))
        (assoc-sibling (make-tree :a))
        (assoc-sibling (make-tree :b))
        (current 0)) => t1))

(facts "About traversing the ordered tree."
  (fact "We should be able to get the label of the top of the tree."
    (label t1) => :r)

  (fact "We should be able to get label of the first-child of the top of the tree."
    (label (first-child t1)) => :a)

  (fact "We should be able to flatten the tree in depth-first-order left->right."
    (flatten label t1) => [:r :a :a :b :a :b :a :a :a :b])

  (fact "Should be able to know the node number of the current node."
    (current (next-sibling (first-child t1))) => 6))

(facts "About the frequencies of subtrees."
  (fact "Should be able to compute all the 1-ary frequent subtrees."
    (let [min-support 0.2
          indexes (fn [map idx tree]
                    (assoc! map tree (conj (map tree []) idx)))]
      (->> (flatten (comp vector label) t1)
           (reduce-kv indexes (transient {}))
           persistent!
           (frequent min-support (size t1)))) => [[[:a] [1 2 4 6 7 8]]
                                                  [[:b] [3 5 9]]]))

(facts "About finding the rightmost-occurances of a tree."
  (rightmost-occurrence t1 [1 2 4 6 7 8] 0 :a) => [2 4 7 8]
  (rightmost-occurrence t1 [2 4 7 8] 1 :a) => [4 8]
  (rightmost-occurrence t1 [2 4 7 8] 1 :b) => [3 5 9]
  (rightmost-occurrence t1 [3 5 9] 1 :b) => [5])

(facts "About expanding subtrees."
  (fact "Expanding a simple 1 node tree should return a tree with 1 child."
    (expand-subtree 0 :b start-a) => (-> start-a
                                         (assoc-child (make-tree :b))))

  (fact "Expanding a 2 node tree on the top node should return a tree
         with one top node and two children."
    (expand-subtree 1 :a (-> start-a
                             (assoc-child (make-tree :b))))
    => (-> start-a
           (assoc-child (make-tree :b))
           (assoc-sibling (make-tree :a))))

  (fact "Expanding a 2 node tree on the second node should return a
  tree with three nodes below each other."
    (expand-subtree 0 :a (-> start-a
                             (assoc-child (make-tree :b))))
    => (-> start-a
           (assoc-child (make-tree :b))
           (assoc-child (make-tree :a))))

  (fact "Expanding a 3 tall tree on the the top node."
    (expand-subtree 2 :c (-> start-a
                             (assoc-child (make-tree :b))
                             (assoc-child (make-tree :a))))
    => (-> start-a
           (assoc-child (make-tree :b))
           (assoc-child (make-tree :a))
           parent
           (assoc-sibling (make-tree :c)))))

(facts "About calculating the successor trees and their rmos."
  (fact "Calculating the successor and rmo for a 1 node tree."
    (calculate-subtree t1 :a [start-a
                              [1 2 4 6 7 8]]) => [[(-> start-a
                                                       (assoc-child (make-tree :a)))
                                                   [2 4 7 8]]])

  (fact
    (calculate-subtree t1 :a [(-> start-a
                                  (assoc-child (make-tree :a)))
                              [2 4 7 8]]) => [[(-> start-a
                                                   (assoc-child (make-tree :a))
                                                   (assoc-sibling (make-tree :a)))
                                               [4 8]]
                                              [(-> start-a
                                                   (assoc-child (make-tree :a))
                                                   (assoc-child (make-tree :a)))
                                               []]]))

(def start-b (OrderedTree. 0 [{:depth 0 :label :b}]))

(facts "About calculating the frequent subtrees."
  (freqt t1 [:a :b] 0.2) => (list start-a
                                  start-b
                                  (assoc-child start-a (make-tree :a))
                                  (assoc-child start-a (make-tree :b))
                                  (-> start-a
                                      (assoc-child (make-tree :a))
                                      (assoc-sibling (make-tree :a)))
                                  (-> start-a
                                      (assoc-child (make-tree :a))
                                      (assoc-sibling (make-tree :b)))
                                  (-> start-a
                                      (assoc-child (make-tree :a))
                                      (assoc-sibling (make-tree :a))
                                      (assoc-sibling (make-tree :b)))))


