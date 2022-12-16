(ns adventofcode.2022.day15)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn- parse-x [part]
  (u/to-int (second (str/split (first (take-last 2 part)) #"[=,]"))))

(defn- parse-y [part]
  (u/to-int (second (str/split (last part) #"="))))

(defn slope [[x1 y1] [x2 y2]]
  (/ (- y2 y1) (- x2 x1)))

(defn create-line [direction [x1 y1] [x2 y2]]
  (let [slope (slope [x1 y1] [x2 y2])
        c (- y1 (* slope x1))]
    {
     :points [[x1 y1] [x2 y2]]
     :direction direction
     :slope slope
     :c c
     :y-intersection-fn (fn [y] [(/ (- y c) slope) y])
     }))

(defn parse-line [line]
  (let [tokens (str/split line #":")
        sensor-part (str/split (first tokens) #" ")
        sensor-x (parse-x sensor-part) sensor-y (parse-y sensor-part)
        beacon-part (str/split (second tokens) #" ")
        beacon-x (parse-x beacon-part) beacon-y (parse-y beacon-part)
        manhattan (u/manhattan-distance [sensor-x sensor-y] [beacon-x beacon-y])
        vertex1 [sensor-x (- sensor-y manhattan)]
        vertex2 [(- sensor-x manhattan) sensor-y]
        vertex3 [sensor-x (+ sensor-y manhattan)]
        vertex4 [(+ sensor-x manhattan) sensor-y]]
    {
     :sensor    [sensor-x sensor-y]
     :beacon    [beacon-x beacon-y]
     :manhattan manhattan
     :lines     [
                 (create-line :left vertex1 vertex2)
                 (create-line :left vertex2 vertex3)
                 (create-line :right vertex3 vertex4)
                 (create-line :right vertex4 vertex1)
                 ]
     :min-x     (- sensor-x manhattan) :max-x (+ sensor-x manhattan)
     :min-y     (- sensor-y manhattan) :max-y (+ sensor-y manhattan)
     }))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day15/" file ".txt"))
        data (map parse-line lines)
        lines (flatten (map :lines data))]
    {:data data :lines lines}))

(defn line-relevant-to-row? [row {[[_ y1] [_ y2]] :points _ :direction}]
  (u/between? row y1 y2))

(defn find-intersection [row {f :y-intersection-fn}]
  (f row))

(defn- sweep [sorted-intersections]
  (reduce
    (fn [[total last-direction last-x] [[x _] direction]]
      (case direction
        :left (case last-direction
                nil [total :left x]
                :left [total :left last-x] ; any left to a previous left can be ignored
                :right [(+ total (- x last-x)) :right x])
        :right (case last-direction
                :left [(+ total (- x last-x)) :left x]
                :right [(+ total (- x last-x)) :right x])))
    [0 nil 0]
    sorted-intersections))

(defn count-empty [{data :data lines :lines} row]
  (let [relevant-lines (filter (partial line-relevant-to-row? row) lines)
        intersections (map #(vector (find-intersection row %) (:direction %)) relevant-lines)
        beacons-on-row (set (filter (fn [[_ y]] (= y row)) (map :beacon data)))
        filtered-intersections (filter #(not (contains? beacons-on-row (first %))) intersections)
        sorted-intersections (sort-by #(-> % first first) filtered-intersections)
        swept (sweep sorted-intersections)
        empty-count (first swept)]
    (println empty-count)
    empty-count))
