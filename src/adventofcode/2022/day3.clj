(ns adventofcode.2022.day3)
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day3/" file ".txt"))]
    lines
    ))

(defn parse-rucksack [rucksack-str]
  (let [size (count rucksack-str)
        half (/ size 2)
        one (subs rucksack-str 0 half)
        two (subs rucksack-str half size)]
    [one two]))

(defn find-shared-item [rucksacks]
  (->> (map set rucksacks)
       (apply set/intersection)
       (first)))

(defn priority [c]
  (let [i (int c)]
    (if (< i 97)
      (+ (- i 65) 27)
      (- i 96))))

(defn part1 [file]
  (->> (parse file)
       (map parse-rucksack)
       (map find-shared-item)
       (map priority)
       (reduce +)))

(defn part2 [file]
  (->> (parse file)
       (partition 3)
       (map find-shared-item)
       (map priority)
       (reduce +)))
