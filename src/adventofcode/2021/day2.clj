(ns adventofcode.2021.day2)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(defn apply-command [[x y] command]
  (let [direction (:direction command)
        value (:value command)]
    (cond
      (= direction :forward) [(+ x value) y]
      (= direction :down) [x (+ y value)]
      (= direction :up) [x (- y value)]
      :else [x y])))

(defn apply-command-part-2 [[aim x y] command]
  (let [direction (:direction command)
        value (:value command)]
    (cond
      (= direction :forward) [aim (+ x value) (+ y (* aim value))]
      (= direction :down) [(+ aim value) x y]
      (= direction :up) [(- aim value) x y]
      :else [x y])))

(defn parse-line [line]
  (let [tokens (str/split line #" ")]
    {:direction (keyword (tokens 0)) :value (u/to-int (tokens 1))}))

(defn solution []
  (let
    [day2-data (u/read-file "./data/2021/day2/input.txt")
     parsed (map parse-line day2-data)]
    {
     :part1 (reduce * (reduce apply-command [0 0] parsed))
     :part2 (let [[aim x y] (reduce apply-command-part-2 [0 0 0] parsed)]
              (* x y))
     }))
