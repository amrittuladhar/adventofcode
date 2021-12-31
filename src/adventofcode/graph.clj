(ns adventofcode.graph
  (:require [adventofcode.util :as u]))
(require '[clojure.data.priority-map :refer [priority-map]])

(defn real-path-with-weights [graph weight-fn reverse-path destination]
  (->> (iterate reverse-path destination)
       (take-while #(not (nil? %)))
       reverse
       (partition 2 1)
       (map (fn [[s d]] [[s d] (weight-fn graph s d)]))))

(defn find-candidate [visited weights]
  (->> weights
       (filter (fn [[node _]] (not (contains? visited node))))
       first
       first))

(defn add-weights [weights new-nodes]
  (reduce
    (fn [map node] (update map node #(if (nil? %) Integer/MAX_VALUE %)))
    weights
    new-nodes))

(defn find-candidate-2 [visited weights graph node children-fn]
  (let [children (children-fn graph node)
        new-weights (add-weights weights children)]
    [(find-candidate visited new-weights)
     new-weights]))

(defn update-weights [candidate candidate-weight-fn]
  (fn [[weights previous] neighbor]
    ; (println "weights:" weights "neighbor:" neighbor)
    (let [current-weight (weights neighbor)
          neighbor-weight (candidate-weight-fn neighbor)
          new-estimate (+ (weights candidate) neighbor-weight)]
      (if (< new-estimate current-weight)
        [(assoc weights neighbor new-estimate)
         (assoc previous neighbor candidate)]
        [weights previous]))))

(defn create-destination-fn [graph destination-def]
  (if (fn? destination-def)
    (partial destination-def graph)
    (partial = destination-def)))

(defn djikstra
  "Uses Djikstra's algorithm to find the shortest path between two nodes in a graph
  Arguments:
  - destination-def - either a
                       - fn with arguments [graph node] that returns true if node is the destination
                       - value that is the destination node
  - weight-fn [graph node1 node2] - fn that returns the weight of the path from node1 to node2
  - children-fn [graph node] - fn the returns the seq of children of a given node"
  [graph src destination-def
   children-fn weight-fn]
  (let [destination-fn (create-destination-fn graph destination-def)]
    (loop [visited #{}
           reverse-path {}
           weights (into (priority-map) [{src 0}])
           next src]
      (let [[candidate new-weights] (find-candidate-2 visited weights graph next children-fn)]
        ; (println "checking" "candidate:" candidate "visited:" visited "weights:" new-weights)
        (cond
          (nil? candidate) '()
          (destination-fn candidate) (real-path-with-weights graph weight-fn reverse-path candidate)
          :else (let [new-visited (conj visited candidate)
                      neighbors (filter #(not (contains? visited %)) (children-fn graph candidate))
                      updated-weights (reduce
                                        (update-weights candidate (partial weight-fn graph candidate))
                                        [new-weights reverse-path]
                                        neighbors)]
                  ; (println "   new" "visited:" new-visited "weights:" new-weights "neighbors:" neighbors)
                  (recur new-visited (second updated-weights) (first updated-weights) (first neighbors))))))))
