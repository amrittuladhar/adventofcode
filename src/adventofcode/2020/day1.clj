(ns adventofcode.2020.day1)
(require '[adventofcode.util :as u])

(defn find-tuple
  [sorted-seq k desired-sum]
  (first (filter #(= (reduce + %) desired-sum) (u/combinations sorted-seq k))))

(defn day1-read-data []
  (vec (sort (map #(u/to-long %) (u/read-file "./data/day/1/input.txt")))))

(defn solution []
  (let [sorted-data (day1-read-data)]
    {:two-sum   (reduce * (find-tuple sorted-data 2 2020))
     :three-sum (reduce * (find-tuple sorted-data 3 2020))}))