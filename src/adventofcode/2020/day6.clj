(ns adventofcode.2020.day6)
(require '[adventofcode.util :as u]
         '[clojure.set :as set])

(def data (u/read-file "./data/day/6/input.txt"))

(defn count-yeses
  "combine-fn is how to combine the data for a group with the answers of the next person in the group"
  [data combine-fn current-group current-count]
  (let [first (first data) rest (rest data)]
    (if (empty? data) (+ current-count (count (set current-group)))
                      (if (empty? first)
                        (count-yeses rest combine-fn nil (+ current-count (count (set current-group))))
                        (count-yeses rest combine-fn (combine-fn current-group first) current-count)))))

(defn count-yeses-everyone
  [data]
  (count-yeses data #(if (nil? %1) (set %2) (set/intersection (set %1) (set %2))) nil 0))

(defn count-yeses-anyone
  [data]
  (count-yeses data concat nil 0))

(defn solution [] {:part1 (count-yeses-anyone data)
               :part2 (count-yeses-everyone data)})