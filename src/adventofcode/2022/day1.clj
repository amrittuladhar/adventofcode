(ns adventofcode.2022.day1)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse [file]
  (as-> (slurp (str "./data/2022/day1/" file ".txt")) data
        (str/split data #"\n\n")
        (map #(str/split % #"\n") data)
        (map #(map u/to-int %) data)))

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
