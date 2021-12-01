(ns adventofcode.2021.day1)
(require '[adventofcode.util :as u])

(defn count-increases
  ([] 0)
  ([current tuple] (if (> (second tuple) (first tuple)) (inc current) current)))

(defn count-sum-increases
  ([] 0)
  ([current tuple] (if (> (reduce + (second tuple)) (reduce + (first tuple))) (inc current) current)))

(defn solution []
  (let [day1-data (map u/to-int (u/read-file "./data/2021/day1/input.txt"))
        groups-of-two (u/partition-seq day1-data 2)
        tuples-of-triples (u/partition-seq (u/partition-seq day1-data 3) 2)]
    {
     :part1 (reduce count-increases 0 groups-of-two)
     :part2 (reduce count-sum-increases 0 tuples-of-triples)
     }
    ))