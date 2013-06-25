(ns rebuild-tree.core-test
  (:use clojure.test
        rebuild-tree.core))

(defn node [value children]
  {:value value
   :children children})

(defn leaf []
  (node :leaf []))

(def initial-tree
  (node "a"
        (list
          (node "b" (list (leaf)
                          (node "d" (list (leaf) (leaf)))))
          (node "c" (list (leaf) (leaf))))))

(def expected-tree
  {:bf-index 0,
   :value "a",
   :children
   (list
     {:bf-index 1,
      :value "b", :children
        (list
          {:value :leaf, :children '()}
          {:bf-index 3,
           :value "d",
           :children
           (list
             {:value :leaf, :children '()}
             {:value :leaf, :children '()})})}
     {:bf-index 2,
      :value "c",
      :children
      (list
        {:value :leaf, :children '()}
        {:value :leaf, :children '()})})})

(deftest integration
  (testing "rebuild the tree"
    (is (= (rebuild-tree initial-tree) expected-tree))))
