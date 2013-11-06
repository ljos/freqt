(ns freqt.core
  (:refer-clojure :exclude [flatten]))

(defprotocol IOrderedTree
  (current [this] [this n]
    "Returns the current node.
     Given a number; sets the current node to `n` and returns it.")
  (depth [this]
    "Returns the depth of the current node.")
  (first-child [this]
    "Sets the current node to the leftmost child of the current node.")
  (assoc-child [this child]
    "Associates the given label to the child node.")
  (assoc-sibling [this sibling]
    "Associates the given label to the sibling node.")
  (label [this]
    "Returns the label for the current node.")
  (next-sibling [this]
    "Sets the current node to the right sibling of the current node.")
  (parent [this]
    "Sets the current node to the parent of this node.")
  (size [this]
    "Returns the size of the whole tree."))

(defrecord OrderedTree [current tree])

(extend-type OrderedTree
  IOrderedTree

  (size [this]
    (count (:tree this)))

  (depth [this]
    (get-in this [:tree (:current this) :depth]))

  (current
    ([this]
       (:current this))
    ([this n]
       (OrderedTree. n (:tree this))))

  (first-child [this]
    (if-let [next (get-in (:tree this)
                    [(:current this) :first-child])]
      (OrderedTree. next (:tree this))))

  (next-sibling [this]
    (if-let [next (get-in (:tree this)
                    [(:current this) :next-sibling])]
      (OrderedTree. next (:tree this))))

  (label [this]
    (get-in (:tree this)
      [(:current this) :label]))

  (parent [this]
    (if-let [next (get-in (:tree this)
                    [(:current this) :parent])]
      (OrderedTree. next (:tree this))))

  (assoc-child [this child]
    (let [current (:current this)
          tree (:tree this)]
      (OrderedTree. (count tree)
                    (-> tree
                        (assoc-in [current :first-child] (count tree))
                        (conj {:depth (inc (get-in tree [current :depth]))
                               :parent current
                               :label child})))))

  (assoc-sibling [this sibling]
    (let [current (:current this)
          tree (:tree this)]
      (OrderedTree. (count tree)
                    (-> tree
                        (assoc-in [current :next-sibling] (count tree))
                        (conj {:depth (get-in tree [current :depth])
                               :parent (get-in tree [current :parent])
                               :label sibling}))))))
(defn make-tree [node]
  (OrderedTree. 0 [{:depth 0 :label node}]))

(defn flatten [f tree]
  (loop [path [tree]
         acc (transient [])]
    (if (empty? path)
      (persistent! acc)
      (if-let [node (peek path)]
        (recur (conj (pop path)
                     (next-sibling node)
                     (first-child node))
               (conj! acc (f node)))
        (recur (pop path) acc)))))

(defn frequent [min-support size rmo]
  (letfn [(remove [rmo [tree support]]
            (if (<= min-support (/ (count support) size))
              (conj! rmo [tree support])
              rmo))]
    (persistent!
     (reduce remove (transient []) rmo))))

(defn nth-parent [node n]
  (if (zero? n) node (recur (parent node) (dec n))))

(defn rightmost-occurrence [tree supports parent l]
  (letfn [(check-nodes [acc node]
            (if (nil? node)
              acc
              (recur (if (= l (label node))
                       (conj acc (current node))
                       acc)
                     (next-sibling node))))]
    (loop [[support & supports] supports
           check nil
           acc []]
      (cond (nil? support)
            acc
            (zero? parent)
            (recur supports
                   check
                   (->> (current tree support)
                        first-child
                        (check-nodes acc)))
            (<= 1 parent)
            (let [node (current tree support)
                  nparent (nth-parent node parent)]
              (if (= check nparent)
                (recur supports
                       check
                       acc)
                (recur supports
                       nparent
                       (->> (nth-parent node (dec parent))
                            next-sibling
                            (check-nodes acc)))))))))

(defn expand-subtree [height label rightmost]
  (if (zero? height)
    (assoc-child rightmost (make-tree label))
    (assoc-sibling (nth-parent rightmost (dec height)) (make-tree label))))

(defn calculate-subtree [tree label [subtree supports]]
  (loop [depth (depth subtree)
         acc []]
    (if (zero? depth)
      (conj acc [(assoc-child subtree (make-tree label))
                 (rightmost-occurrence tree supports 0 label)])
      (recur (dec depth)
             (conj acc [(assoc-sibling (nth-parent subtree (dec depth)) (make-tree label))
                        (rightmost-occurrence tree supports depth label)])))))


(defn calculate-subtrees [tree labels freqs]
  (apply concat
         (for [label labels
               freq freqs]
           (calculate-subtree tree label freq))))

(defn freqt [tree labels min-support]
  (loop [freqt (let [freqs (reduce-kv (fn [freqs idx label]
                                        (assoc freqs label (conj (freqs label []) idx)))
                                      {}
                                      (flatten label tree))]
                 (->> (for [label labels]
                        [(make-tree label)
                         (freqs label [])])
                      (frequent 0.2 (size tree))))
         acc '()]
    (if (empty? freqt)
      acc
      (recur (frequent 0.2 (size tree) (calculate-subtrees tree labels freqt))
             (concat acc (map first freqt))))))
