(ns adventofcode.2022.day8)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def directions
  [
   [:east [1 0]],
   [:west [-1 0]],
   [:north [0 -1]],
   [:south [0 1]]
   ])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day8/" file ".txt"))]
    (u/parse-into-matrix lines u/to-int)))

(defn move [dir [x y]]
  (let [[xx yy] (second (nth directions dir))]
    [(+ x xx) (+ y yy)]))

(defn move-until [mat move-fn [x y] check-fn]
  (loop [[x y] (move-fn [x y])]
    ;(println "move-until" x y)
    (cond
      (not (u/inside-matrix? mat x y)) :outside
      (check-fn (u/find-in-matrix mat x y)) :failed
      :else (recur (move-fn [x y])))))


(defn is-visible? [mat [x y]]
  (loop [dir 0]
    (let [move-fn (partial move dir)]
      (cond
        (> dir 3) false                                     ; we've run out of directions to pursue
        :else (let [move-result (move-until mat move-fn [x y] #(>= % (u/find-in-matrix mat x y)))]
                (cond
                  (= move-result :outside) true
                  :else (recur (inc dir))))))))

(defn part1 [file]
  (let [mat (parse file)
        {x :x y :y} (u/matrix-size mat)]
    (->> (for [xx (range 0 x) yy (range 0 y)]
           [[xx yy] (is-visible? mat [xx yy])])
         (map second)
         (filter true?)
         count)))

; Part 2

(defn scenic-score-dir [mat move-fn [x y]]
  (let [height (u/find-in-matrix mat x y)]
    (loop [[x y] (move-fn [x y]) score 0]
      ;(println "move-until" x y)
      (cond
        (not (u/inside-matrix? mat x y)) score
        (>= (u/find-in-matrix mat x y) height) (inc score)
        :else (recur (move-fn [x y]) (inc score))))))

(defn scenic-score [mat [x y]]
  (loop [dir 0 score 1]
    (let [move-fn (partial move dir)]
      (cond
        (> dir 3) score
        :else (let [dir-score (scenic-score-dir mat move-fn [x y])]
                (recur (inc dir) (* score dir-score)))))))

(defn part2 [file]
  (let [mat (parse file)
        {x :x y :y} (u/matrix-size mat)]
    (->> (for [xx (range 0 x) yy (range 0 y)]
           [[xx yy] (scenic-score mat [xx yy])])
         (map second)
         (apply max))))