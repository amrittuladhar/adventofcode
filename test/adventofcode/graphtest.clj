(ns adventofcode.graphtest)
(require '[adventofcode.graph :as g]
         '[clojure.test :refer [deftest is]])

(def graph {:a {:b 7 :c 3}
            :b {:c 1 :d 2 :e 6}
            :c {:d 2}
            :d {:e 4}
            :e {}})

(defn children [graph node]
  (lazy-seq (concat
              ((fnil keys {}) (graph node))
              (map first (select-keys graph (for [[k v] graph :when (some #(= % node) (keys v))] k))))))

(defn nodes [graph]
  (keys graph))

(defn weight [graph node1 node2]
  (first
    (filter
      #(not (nil? %))
      (list
        (get-in graph [node1 node2])
        (get-in graph [node2 node1])))))

(deftest djikstra
  (is (= (g/djikstra graph :c :e children weight)
         '([[:c :d] 2] [[:d :e] 4])))
  (is (= (g/djikstra graph :c #(= %2 :e) children weight)
         '([[:c :d] 2] [[:d :e] 4]))))