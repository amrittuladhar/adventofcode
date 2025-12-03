(ns adventofcode.2025.day2
  (:require [clojure.math :as math]))
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2025/day2/" file ".txt"))]
    (as-> lines x
          (first x)
          (str/split x #",")
          (map #(str/split % #"-") x)
          (map #(vector (u/to-long (first %)) (u/to-long (second %))) x))))

(defn -find-invalid-part-1 [length min max]
  ;(println min max length)
  (cond
    (= (rem length 2) 1) '()
    :else (let [first-half (subs (str min) 0 (/ length 2))]
            (loop [lst '() half-num first-half]
              ; (println "count " count " half-num" half-num)
              (let [current-invalid (u/to-long (str half-num half-num))]
                (cond
                  (= current-invalid max) (conj lst current-invalid)
                  (> current-invalid max) lst
                  (< current-invalid min) (recur lst (inc (u/to-long half-num)))
                  :else (recur (conj lst current-invalid) (inc (u/to-long half-num)))))))))

(defn first-number [length]
  (int (math/pow 10 (dec length))))

(defn last-number [length]
  (- (first-number (inc length)) 1))

; form ranges by length -- e.g. [95 - 2005] will yield [95, 99], [100, 999], [1000, 2005]
(defn form-ranges-by-length [[min max]]
  (let [max-length (count (str max))]
    (loop [minn min ranges []]
      ; (println "minn " minn " ranges " ranges)
      (let [cur-length (count (str minn))]
        (cond
          (= cur-length max-length) (conj ranges (vector minn max))
          :else (recur
                  (first-number (inc cur-length))           ; minn
                  (conj ranges (vector minn (last-number cur-length)))))))))

(defn part1 [file]
  (let [ranges (parse file)]
    (->> ranges
         (map form-ranges-by-length)
         (apply concat)                                     ; "flatten by one level"
         (map (fn [range] (-find-invalid-part-1
                            (count (str (first range)))
                            (first range)
                            (second range))))
         flatten
         (reduce +))))

; Part 2

(defn has-repeating-pattern? [s]
  (let [answer (not (nil? (re-matches #"^(.+?)\1+$" s)))]
    ; (println s answer)
    answer))

(defn -find-invalid-part-2 [length min max]
  (->> (range min (inc max))
       (map str)
       (filter has-repeating-pattern?)
       (map u/to-long)))

(defn part2 [file]
  (let [ranges (parse file)]
    (->> ranges
         (map form-ranges-by-length)
         (apply concat)                                     ; "flatten by one level"
         (map (fn [range] (-find-invalid-part-2
                            (count (str (first range)))
                            (first range)
                            (second range))))
         flatten
         (reduce +))))
