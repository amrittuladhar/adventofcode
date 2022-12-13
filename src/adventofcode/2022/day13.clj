(ns adventofcode.2022.day13)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def IN_ORDER -1)
(def OUT_OF_ORDER 1)

(defn ordered? [left right]
  ; (println "comparing" left right)
  (cond
    (and (number? left) (number? right))
    (cond
      (< left right) IN_ORDER
      (> left right) OUT_OF_ORDER
      (= left right) :continue)
    (and (vector? left) (number? right)) (ordered? left (vector right))
    (and (number? left) (vector? right)) (ordered? (vector left) right)
    :else (loop [left left right right]
            (let [left-head (first left) right-head (first right)]
              ; (println "left-head" left-head "right-head" right-head)
              (cond
                (nil? left-head) (if (nil? right-head) :continue IN_ORDER)
                (nil? right-head) OUT_OF_ORDER
                :else (let [result (ordered? left-head right-head)]
                        ; (println "result" result)
                        (case result
                          -1 IN_ORDER
                          1 OUT_OF_ORDER
                          :continue (recur (rest left) (rest right)))))))))


(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day13/" file ".txt"))]
    (->> lines
         (filter #(not (empty? %)))
         (map read-string))))

(defn part1 [file]
  (let [data (parse file)]
    (->> (partition 2 2 data)
        (map (fn [pair] (ordered? (first pair) (second pair))))
         u/zip-indexed
         (filter (fn [[_ val]] (= val IN_ORDER)))
         (map first)
         (map inc)
         (reduce +))))

(def DECODERS #{[[2]] [[6]]})
(defn part2 [file]
  (let [data (parse file)
        packets (concat data DECODERS)]
    (->> packets
         (sort #(ordered? %1 %2))
         u/zip-indexed
         (filter #(contains? DECODERS (second %)))
         (map first)
         (map inc)
         (reduce *))))
