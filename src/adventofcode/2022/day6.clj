(ns adventofcode.2022.day6)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day6/" file ".txt"))]
    (first lines)))

(defn find-marker [message length]
  (->> (partition length 1 message)
       (take-while #(not (= length (count (set %)))))
       count
       (+ length)))

(defn part1 [file]
  (find-marker (parse file) 4))

(defn part2 [file]
  (find-marker (parse file) 14))