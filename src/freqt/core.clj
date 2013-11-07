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

(defn- tree->vec [old-size old-depth elem]
  (loop [elem elem
         tree []]
    (if (<= (size elem)
            (current elem))
      tree
      (let [l (label elem)
            curr (current elem)
            p (if-let [p (parent elem)] (current p))
            fc (if-let [c (first-child elem)] (current c))
            ns (if-let [n (next-sibling elem)] (current n))
            d (depth elem)]
        (recur (current elem (inc curr))
               (conj tree
                     (into {} `[[:depth ~(if d (+ d old-depth))]
                                ~@(if p (list [:parent (+ p old-size)]))
                                [:label ~l]
                                ~@(if fc (list [:first-child (+ fc old-size)]))
                                ~@(if ns (list [:next-sibling (+ ns old-size)]))])))))))

(extend-protocol IOrderedTree
  OrderedTree
  (size [this]
    (count (:tree this)))

  (depth [{:keys [tree current]}]
    (get-in tree [current :depth]))

  (current
    ([this]
       (:current this))
    ([this n]
       (OrderedTree. n (:tree this))))

  (first-child [{:keys [tree current]}]
    (if-let [next (get-in tree [current :first-child])]
      (OrderedTree. next tree)))

  (next-sibling [{:keys [current tree]}]
    (if-let [next (get-in tree [current :next-sibling])]
      (OrderedTree. next tree)))

  (label [{:keys [tree current]}]
    (get-in tree [current :label]))

  (parent [{:keys [current tree]}]
    (if-let [next (get-in tree [current :parent])]
      (OrderedTree. next tree)))

  (assoc-child [{:keys [current tree] :as this} child]
    (if (nil? child)
      this
      (let [addition (tree->vec (size this) (inc (depth this)) child)]
        (OrderedTree. (count tree)
                      (into (assoc-in tree [current :first-child]
                              (count tree))
                            (assoc-in addition [0 :parent]
                              current))))))

  (assoc-sibling [{:keys [current tree] :as this} sibling]
    (if (nil? sibling)
      this
      (let [addition (tree->vec (size this) (depth this) sibling)
            parent (get-in tree [current :parent])]
        (OrderedTree. (count tree)
                      (into (assoc-in tree [current :next-sibling]
                              (count tree))
                            (assoc-in addition [0 :parent] parent)))))))

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
  (let [freqs (let [freqs (reduce-kv (fn [f idx l]
                                        (assoc f l (conj (f l []) idx)))
                                      {}
                                      (flatten label tree))]
                (->> (for [l labels]
                       [(make-tree l) (freqs l [])])
                     (frequent min-support (size tree))))]
    (loop [freqt freqs
           acc '()]
      (if (empty? freqt)
        acc
        (recur (frequent min-support
                         (size tree)
                         (calculate-subtrees tree labels freqt))
               (concat acc (map first freqt)))))))
