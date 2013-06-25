(ns rebuild-tree.core
  (use clojure.set)
  (require [clojure.pprint :as pp]))


(defn leaf? [node]
  (= (:value node) :leaf))

(defn list-append [l element]
  (apply list (conj (vec l) element)))

(defn bf-traverse
  ([tree] (bf-traverse [] [tree]))
  ([nodes-so-far next-rung]
    (if (empty? next-rung)
      nodes-so-far
      (bf-traverse
        (concat nodes-so-far next-rung)
        (flatten (map :children next-rung))))))

(defn inc-index [index node]
  (if (leaf? node)
    index
    (inc index)))

(defn with-bf-index
  ([nodes]
   (with-bf-index nodes 0 '()))
  ([remaining-nodes index nodes-so-far]
   (if (empty? remaining-nodes)
     (reverse nodes-so-far)
     (let [to-update (first remaining-nodes)
           updated-node (if (leaf? to-update) to-update (assoc to-update :bf-index index))]
       (recur (rest remaining-nodes) (inc-index index to-update) (conj nodes-so-far updated-node))))))

(defn squash-node [node]
  (let [with-child-count (assoc node :child-count (count (:children node)))
        without-children (assoc with-child-count :children '())]
    without-children
    ))

(defn squash-nodes [nodes]
  (map squash-node
       nodes))

(defn update-tree [matcher update-fn node]
  (if (matcher node)
    (update-fn node)
    (let [updated-descendents (map
                                #(update-tree matcher update-fn %)
                                (:children node))]
      (assoc
        node
        :children
        updated-descendents))))

(defn first-wanting-node [tree]
  (let [traversal (bf-traverse tree)]
    (first (filter #(pos? (- (:child-count %) (count (:children %)))) traversal))))

(defn add-to-tree [tree-so-far new-node]
  (let [target-bf-index (:bf-index (first-wanting-node tree-so-far))
        matcher #(= (:bf-index %) target-bf-index)
        update-fn #(assoc % :children (list-append (:children %) new-node))]
    (update-tree matcher update-fn tree-so-far)))

(defn clean-child-counts [t]
  (let [updated-root (dissoc t :child-count)
        updated-children (map clean-child-counts (:children t))]
    (assoc updated-root :children updated-children)))

(defn rebuild-tree [t]
  (let [traversal (bf-traverse t)
        indexed-traversal (with-bf-index traversal)
        squashed-traversal (squash-nodes indexed-traversal)
        rebuilt (reduce add-to-tree squashed-traversal)
        without-child-counts (clean-child-counts rebuilt)
        ]
    without-child-counts))
