(ns adventofcode.2021.day7)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

; Find median - used for part 1
; accurate only for odd number of items!
(defn median [coll]
  (nth (sort coll) (/ (count coll) 2)))

; 1 + 2 + ... + |x-y| = ((|x-y|)(|x-y|+1) / 2)
(defn find-fuel-to-travel [x y]
  (cond
    (= x y) 0
    :else (let [distance (Math/abs ^int (- x y))]
            (/ (* distance (inc distance)) 2))))

(defn find-minimum-total-fuel [distances]
  (let [a (apply min distances)
        b (apply max distances)
        sorted (sort distances)]
    (apply min (map (fn [check] (reduce + (map #(find-fuel-to-travel check %) sorted))) (range a (inc b))))))

(defn solution []
  (let [data (u/read-file "./data/2021/day7/input.txt")
        distances (map u/to-int (str/split (first data) #","))]
    (let [median (median distances)]
      {
       :part1 (reduce + (map (fn [distance] (Math/abs ^int (- median distance))) distances))
       :part2 (find-minimum-total-fuel distances)
       })))
