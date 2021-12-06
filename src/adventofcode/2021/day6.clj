(ns adventofcode.2021.day6)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

; Part 1
(def evolve-fish
  (memoize (fn [fish]
             (cond
               (= fish 0) (list 6 8)
               :else (list (dec fish))))))

(defn evolve [days fishes]
  (loop [days days
         fishes fishes]
    (cond
      (= days 0) fishes
      :else (recur (dec days) (flatten (map evolve-fish fishes))))))

; Optimized for part 2
(def count-evolved-fish
  (memoize (fn [days fish]
             (cond
               (= days 0) 1
               :else (reduce + 0 (map #(count-evolved-fish (dec days) %) (evolve-fish fish)))))))

(defn count-fish [days fishes]
  (let
    [grouped-fish (frequencies fishes)] ; only calculate for unique starting points
      (reduce + (map
                  (fn [[fish how-many]]
                    (* (count-evolved-fish days fish) how-many))
                  grouped-fish))))

(defn solution []
  (let [data (first (u/read-file "./data/2021/day6/input.txt"))
        fishes (map u/to-long (str/split data #","))]
    {
      ;:part1 (count (evolve 80 fishes)) ; Slow version
      :part1 (count-fish 80 fishes)
      :part2 (count-fish 256 fishes)
     }))