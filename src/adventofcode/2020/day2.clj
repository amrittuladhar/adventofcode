(ns adventofcode.2020.day2)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-rule
  [rule]
  (let [tokens (str/split rule #" ")
        limits (tokens 0)
        letter (tokens 1)
        limit-numbers (str/split limits #"-")]
    {:letter (first letter) ; convert to char!
     :rule1  (u/to-int (limit-numbers 0))
     :rule2  (u/to-int (limit-numbers 1))}))

(defn apply-rule-part1
  ([rule password]
   (let [count (u/count-letter password (:letter rule))]
     (and
       (>= count (:rule1 rule))
       (<= count (:rule2 rule)))))
  ([parsed-line]
   (apply-rule-part1 (:rule parsed-line) (:password parsed-line))))

(defn apply-rule-part2
  ([rule password]
   (let [index1 (:rule1 rule)
         index2 (:rule2 rule)
         letter (:letter rule)
         index1-matches (u/check-character password index1 letter)
         index2-matches (u/check-character password index2 letter)]
     (or
       (and index1-matches (not index2-matches))
       (and index2-matches (not index1-matches)))))
  ([parsed-line]
   (apply-rule-part2 (:rule parsed-line) (:password parsed-line))))

(defn parse-line
  [line]
  (let [tokens (str/split line #":")
        rule (tokens 0)
        password (str/trim (tokens 1))]
    {:rule     (parse-rule rule)
     :password password}))

(defn solution []
  (let [day2-data (u/read-file "./data/day/2/input.txt")
        parsed-lines (map parse-line day2-data)]
    {:part1 (count (filter apply-rule-part1 parsed-lines))
     :part2 (count (filter apply-rule-part2 parsed-lines))}))
