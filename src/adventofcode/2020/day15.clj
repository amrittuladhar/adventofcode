(ns adventofcode.2020.day15)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(def data-1 '(0, 3, 6))
(def data-2 '(1, 3, 2))
(def data '(1 20 11 6 12 0))

(defn next-spoken-number
  [previous occurrences]
  (let [previous-occurrences (occurrences previous)]
    (if (= (count previous-occurrences) 1)
      0
      (- (first previous-occurrences) (second previous-occurrences)))))

(defn spoken-number
  "Find the nth spoken number"
  [starting-numbers n next-spoken-number-fn]
  (if (< n (count starting-numbers))
    (nth starting-numbers n)
    ; occurrences is map from number to a vector indicating the 0-indexed positions at which the
    ; number was spoken
    (loop [occurrences (into {} (map-indexed #(vector %2 (list %1)) starting-numbers))
           prev (last starting-numbers)
           current-index (count starting-numbers)]
      ;(printf "occurrences: %s prev: %s current-index: %s\n"
      ;        occurrences prev current-index)
      (cond
        (= current-index (inc n)) prev
        :else (let [new-value (next-spoken-number-fn prev occurrences)
                    new-occurrences (update-in occurrences [new-value] (fn [seq] (conj seq current-index)))]
                (recur new-occurrences new-value (inc current-index)))))))

(defn solution
  ([starting-numbers n]
   (spoken-number starting-numbers n next-spoken-number))
  ([]
   ; note that answers are 0-indexed so for 2020 we ask for 2019
   {:part1 (spoken-number data 2019 next-spoken-number)
    :part2 (spoken-number data 29999999 next-spoken-number)}))

; 30000000
; 29999999