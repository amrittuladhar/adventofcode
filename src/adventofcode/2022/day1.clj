(ns adventofcode.2022.day1)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse [file]
  (loop [lines (u/read-file (str "./data/2022/day1/" file ".txt"))
         rations []
         current []]
    (if
      (empty? lines)
      rations
      (let [line (first lines)]
        (if (empty? line)
          (recur (rest lines) (conj rations current) [])
          (recur (rest lines) rations (conj current (u/to-int line))))))))

(defn sums [data]
  (map #(reduce + %) data))

(defn part1 [file]
  (let [data (parse file)
         sums (sums data)]
    (reduce max sums)))

(defn part2 [file]
  (let [data (parse file)
        sums (sums data)]
    (reduce + (take 3 (sort #(compare %2 %1) sums)))))
