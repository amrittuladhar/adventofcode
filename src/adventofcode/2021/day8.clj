(ns adventofcode.2021.day8
  (:require [clojure.set :as set]))
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def ORIGINAL
  ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"])

(def ORIGINAL-BY-SIZE (group-by count ORIGINAL))

(defn pattern-to-number [pattern]
  (let [indexed-originals (map-indexed (fn [index item] (vector index item)) ORIGINAL)]
    (first (first (filter (fn [[_ item]] (= item pattern)) indexed-originals)))))

(defn solved? [solution-entry]
  (= (count (second solution-entry)) 1))

(defn remove-knowns [solution]
  (let [known (filter solved? solution)
        known-keys (set (keys known))
        known-values (set (flatten (map second known)))
        unknown-keys (filter (fn [segment] (not (contains? known-keys segment))) (keys solution))]
    ; (println "solution: " solution "\n\tknown: " known "\n\tknown values: " known-values "\n\tunknown keys:" unknown-keys)
    (u/update-vals
      solution
      unknown-keys
      (fn [existing] (filter #(not (contains? known-values %)) existing)))))

(defn update-with-pattern [solution pattern]
  ; (println "solution: " solution "\n\tpattern: " pattern)
  (let [possible-original-patterns (ORIGINAL-BY-SIZE (count pattern))
        ; only for segments that are present in all the possible original patterns can eliminate possible mappings
        possible-original-segments (apply set/intersection (map set possible-original-patterns))]
    ; (println "\n\toriginal: " possible-original-patterns "\n\tsegments: " possible-original-segments)
    (u/update-vals
      solution
      possible-original-segments
      (fn [existing] (if (empty? existing)
                       (seq pattern)
                       (seq (set/intersection (set existing) (set pattern))))))))

(defn solve [patterns]
  (loop [patterns patterns count 1
         solution (u/into-map '(\a \b \c \d \e \f \g) '())]
    (cond
      (every? solved? solution) (u/update-vals solution (keys solution) first) ; Solved - solution will have values as list so unwrap
      (= count 500) solution                                ; stop-gap to avoid infinite recursion
      (empty? patterns) (recur nil (inc count) (remove-knowns solution))
      :else (let [pattern (first patterns)]
              (recur
                (rest patterns) (inc count)
                (-> solution
                    remove-knowns
                    (update-with-pattern pattern)))))))

(defn parse [line]
  (let [tokens (map str/trim (str/split line #"\|"))
        patterns (map str/trim (str/split (nth tokens 0) #" "))
        digits (map str/trim (str/split (nth tokens 1) #" "))]
    {:patterns patterns :outputs digits}))

(defn fix-output [solution-reversed output]
  (map solution-reversed output))

(defn find-output-values [input]
  (let [patterns (:patterns input)
        outputs (:outputs input)
        solution (solve patterns)
        solution-reversed (u/reverse-map solution)]
    (map #(pattern-to-number (str/join (sort (fix-output solution-reversed %)))) outputs)))


(defn solution []
  (let [data (u/read-file "./data/2021/day8/input.txt")
        parsed (map parse data)]
    {
     :part1 (->> parsed
                 (map :outputs)
                 flatten
                 (filter (fn [outputs] (let [c (count outputs)] (contains? #{2 3 4 7} c))))
                 count)
     :part2 (reduce + (map #(u/to-int (str/join (find-output-values %))) parsed))}))


; Notes working out the elimination pattern

; be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe

; cf -> be
; abcdefg -> cfbegad
; (abcefg / abdefg) -> cbdgef

; c -> (b, e)
; f -> (b, e)

; a -> (c, f, b, e, g, a, d)
; b -> (c, f, b, e, g, a, d)
; c -> (b, e)
; d -> (c, f, b, e, g, a, d)
; e -> (c, f, b, e, g, a, d)
; f -> (c, f, b, e, g, a, d)
; g -> (c, f, b, e, g, a, d)

; a -> (c, f, b, e, g, d)
; b -> (c, f, b, e, g, d)
; c -> (b, e)
;
