(ns adventofcode.2025.day1)
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.math :as math])

(defn parse-line [line]
  (let [dir (subs line 0 1)
        rot (u/to-int (subs line 1))]
    {:dir dir :rot rot}))

(defn rotate [{dir :dir rot :rot}]
  (cond
    (= dir "L") (- rot)
    :else rot))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2025/day1/" file ".txt"))]
    (map parse-line lines)))

(defn count-zeroes [initial rotations count-fn]
  (let [initial-conditions {:count 0 :current initial}
        result (reduce (fn [{count :count current :current} rotation]
                         (let [new-current (mod (+ rotation current) 100)]
                           {
                            :count   (count-fn count current rotation new-current)
                            :current new-current
                            }
                           )
            ) initial-conditions rotations)]
    (:count result)))

(defn part1 [file]
  (let [rotations (map rotate (parse file))]
    (count-zeroes 50 rotations (fn [count current _ _] (if (= current 0) (inc count) count)))))

(defn count-zero-crosses [rotation current]
  (cond
    ; if going left, number of times we cross 0 = (rotations + current \ 100)
    (>= rotation 0) (quot (+ rotation current) 100)
    ; if going right, we take mirror image and do the above -- couldn't figure out a simpler way
    :else (count-zero-crosses (- rotation) (mod (- 100 current) 100))))

(defn count-part-2 [count current rotation new-current]
    (+ count (count-zero-crosses rotation current)))

(defn part2 [file]
  (let [rotations (map rotate (parse file))]
    (count-zeroes 50 rotations count-part-2)))
