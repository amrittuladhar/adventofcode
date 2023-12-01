(ns adventofcode.2023.day1)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-line [line]
  (let [rev (reverse line)]
    (+ (* (u/to-int (first (filter #(Character/isDigit (char %)) line))) 10)
       (u/to-int (first (filter #(Character/isDigit (char %)) rev))))))

(def DIGITS
  [["1" 1]
   ["2" 2]
   ["3" 3]
   ["4" 4]
   ["5" 5]
   ["6" 6]
   ["7" 7]
   ["8" 8]
   ["9" 9]
   ["one" 1]
   ["two" 2]
   ["three" 3]
   ["four" 4]
   ["five" 5]
   ["six" 6]
   ["seven" 7]
   ["eight" 8]
   ["nine" 9]])

(defn find-digits [s index-fn]
  (->> DIGITS
       (map (fn [digit] (vector (second digit) (index-fn s (first digit)))))
       (filter #(not (nil? (second %))))))

(defn parse-line-2 [s]
  (let [numbers (concat (find-digits s str/index-of) (find-digits s str/last-index-of))
        sorted (sort-by second numbers)
        f (first (first sorted))
        l (first (last sorted))
        d (+ (* 10 f) l)]
    d))

(defn parse [file]
  (as-> (slurp (str "./data/2023/day1/" file ".txt")) data
        (map parse-line (str/split data #"\n"))))

(defn parse-2 [file]
  (as-> (slurp (str "./data/2023/day1/" file ".txt")) data
        (map parse-line-2 (str/split data #"\n"))))

(defn part1 [file]
  (let [data (parse file)]
    (reduce + data)))

(defn part2 [file]
  (let [data (parse-2 file)]
    (reduce + data)))
