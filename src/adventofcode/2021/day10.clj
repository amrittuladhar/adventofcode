(ns adventofcode.2021.day10)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def SCORES {\) 3 \] 57 \} 1197 \> 25137})
(def SCORES-2 {\) 1 \] 2 \} 3 \> 4})
(def COMPLETIONS {\( \) \[ \] \{ \} \< \>})

(def PAIRS {\( \) \[ \] \{ \} \< \>})

; Find median - used for part 2
; this looks familiar...
(defn median [coll]
  (nth (sort coll) (/ (count coll) 2)))

(defn first-corrupted [line]
  (loop [line line stack []]
    (let [head (first line)]
      (cond
        (empty? line) nil
        (contains? PAIRS head) (recur (rest line) (conj stack head))
        (= head (PAIRS (last stack))) (recur (rest line) (pop stack))
        :else head))))

; modified "first-corrupted" for part 2
(defn remaining [line]
  (loop [line line stack []]
    (let [head (first line)]
      (cond
        (empty? line) stack
        (contains? PAIRS head) (recur (rest line) (conj stack head))
        (= head (PAIRS (last stack))) (recur (rest line) (pop stack))
        :else nil))))

(defn complete [line]
  (map COMPLETIONS (reverse (remaining line))))

(defn filter-nil [coll]
  (filter #(not (nil? %)) coll))

(defn score-part-2 [solution]
  (loop [solution solution
         score 0]
    (cond
      (empty? solution) score
      :else (let [new-score (+ (* 5 score) (SCORES-2 (first solution)))]
              (recur (rest solution) new-score)))))

(defn solution []
  (let [data (u/read-file "./data/2021/day10/input.txt")]
    {
     :part1 (->> data
                 (map first-corrupted)
                 (filter #(not (nil? %)))
                 (map SCORES)
                 (reduce +))

     :part2 (->> data
                 (map complete)
                 (filter #(not (empty? %)))
                 (map score-part-2)
                 median)
     }))
