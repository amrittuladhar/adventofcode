(ns adventofcode.2022.day4)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-seats [seats]
  (vec (map u/to-int (str/split seats #"-"))))

(defn parse-line [line]
  (let [pairs (str/split line #",")]
    (map parse-seats pairs)))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day4/" file ".txt"))]
    (map parse-line lines)))

(defn ordered? [a b c]
  (and (>= b a) (<= b c)))

(defn fully-contains? [[a b] [x y]]
  (or
    (and (>= x a) (<= y b))
    (and (>= a x) (<= b y))))

(defn overlaps? [[a b] [x y]]
  (or
    (ordered? a x b)
    (ordered? a y b)
    (ordered? x a y)
    (ordered? x b y)))

(defn part1 [file]
  (let [data (parse file)]
    (count (filter #(fully-contains? (first %) (second %)) data))))

(defn part2 [file]
  (let [data (parse file)]
    (count (filter #(overlaps? (first %) (second %)) data))))
