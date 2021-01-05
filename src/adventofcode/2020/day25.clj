(ns adventofcode.2020.day25)
(require '[adventofcode.util :as u])

(defn- calculate-public-key-or-loop-size
  [start-number divisor public-key iterations]
  (loop [current-public-key 1
         current-iterations 0]
    (cond
      (= current-public-key public-key) current-iterations
      (= current-iterations iterations) current-public-key
      :else (recur (mod (* current-public-key start-number) divisor) (inc current-iterations)))))

(defn calculate-loop-size
  [subject-number divisor public-key]
  (calculate-public-key-or-loop-size subject-number divisor public-key nil))

(defn calculate-key
  [start-number divisor iterations]
  (calculate-public-key-or-loop-size start-number divisor nil iterations))

(def data (map u/to-int (u/read-file "./data/day/25/input.txt")))

(defn solve
  ([data]
   (let [subject-number 7
         divisor 20201227
         public-key-1 (first data) public-key-2 (second data)]
     (let [loop-size-1 (calculate-loop-size subject-number divisor public-key-1)]
       {:part1 (calculate-key public-key-2 divisor loop-size-1)})))
  ([] (solve data)))