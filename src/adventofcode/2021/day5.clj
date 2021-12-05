(ns adventofcode.2021.day5)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn inclusive-range [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn find-points-between [[x1 y1] [x2 y2]]
  (mapv identity (u/zip-with-pad (inclusive-range x1 x2) (inclusive-range y1 y2))))

(defn parse-coords [coords-string]
  (mapv u/to-int (str/split (str/trim coords-string) #",")))

(defn parse-input [input-line]
  (mapv parse-coords (str/split input-line #"->")))

(defn line-count-reducer [count-map next-parsed-line]
  ;(println
  ;  next-parsed-line
  ;  "x: " (first next-parsed-line)
  ;  " y: " (second next-parsed-line)
  ;  " b: " (find-points-between (first next-parsed-line) (second next-parsed-line)))
  (u/update-vals
    count-map
    (find-points-between (first next-parsed-line) (second next-parsed-line))
    #(if (nil? %) 1 (inc %))))

(defn count-lines [parsed-inputs]
  (reduce line-count-reducer {} parsed-inputs))

(defn part1-filter [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn solution []
  (let [data (u/read-file "./data/2021/day5/input.txt")
        parsed-lines (map parse-input data)
        part1-lines (filter part1-filter parsed-lines)
        part1-count-map (count-lines part1-lines)
        part2-count-map (count-lines parsed-lines)]
    {
     :part1 (count (filter #(> (second %) 1) part1-count-map))
     :part2 (count (filter #(> (second %) 1) part2-count-map))
     }))