(ns adventofcode.2022.day14)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn line-coords [[x1 y1] [x2 y2]]
  ; (println "line-coords" [x1 y1] [x2 y2])
  (cond
    (= x1 x2) (for [y (u/range-inc y1 y2)] [x1 y])
    (= y1 y2) (for [x (u/range-inc x1 x2)] [x y1])))

(defn parse-line [line]
  (->> (str/split line #"->")
       (map str/trim)
       (map #(str/split % #","))
       (map #(vec (map u/to-int %)))                        ; = ([1 2] [1 5] [6 5])
       (partition 2 1)                                      ; = (([1 2] [1 5]) ([1 5] [6 5]))
       (map #(apply line-coords %))))                       ; = (([1 2] [1 3] [1 4] [1 5]) ([1 5] [2 5] [3 5] [4 5] [5 5] [6 5]))

(defn- unwrap [coords]
  (->> coords
       (apply concat)
       (apply concat)))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day14/" file ".txt"))
        rocks (unwrap (map parse-line lines))]
    (->> rocks
         (map #(vector % \#))
         (u/pairs-to-map))))

(def path
  '([0 1]                                                   ; straight down
    [-1 1]                                                  ; down-left
    [1 1]))                                                 ; down-right

(defn fill [cave max-y [initial-sand-x initial-sand-y]]
  (loop [cave cave
         sand-x initial-sand-x
         sand-y initial-sand-y]
    (let [all-possible (map (fn [[xx yy]] [(+ sand-x xx) (+ sand-y yy)]) path)
          next (first (filter #(nil? (cave %)) all-possible))]
      ; (println max-y "sand:" [sand-x sand-y] "next:" next)
      (cond
        (nil? next) (recur (assoc cave [sand-x sand-y] \o) initial-sand-x initial-sand-y) ; nowhere to go so stop
        (> (second next) max-y) cave                        ; this grain has moved outside known cave so we're done
        :else (recur cave (first next) (second next))))))

(defn part1 [file]
  (let [cave (parse file)
        ys (map second (map first cave))
        max-y (apply max ys)]
    (->> (fill cave max-y [500 0])
         (filter #(= (second %) \o))
         count)))

; Part 2

(defn fill-2 [cave floor-y [initial-sand-x initial-sand-y]]
  (loop [cave cave
         sand-x initial-sand-x
         sand-y initial-sand-y]
    (let [all-possible (map (fn [[xx yy]] [(+ sand-x xx) (+ sand-y yy)]) path)
          next (first (filter #(nil? (cave %)) all-possible))]
      ; (println floor-y "sand:" [sand-x sand-y] "next:" next)
      (cond
        (nil? next)                                         ; nowhere to go
        (cond
          ; nowhere to go because we're at the top - we can stop
          (= [sand-x sand-y] [initial-sand-x initial-sand-y]) (assoc cave [sand-x sand-y] \o)
          ; nowhere to go because we're at the floor - drop another grain of sand
          (= (second next) floor-y)) (recur (assoc cave [sand-x sand-y] \o) initial-sand-x initial-sand-y)
        :else (recur cave (first next) (second next))))))

(defn part2 [file]
  (let [cave (parse file)
        ys (map second (map first cave))
        floor-y (+ 2 (apply max ys))]
    (->> (fill-2 cave floor-y [500 0])
         (filter #(= (second %) \o))
         count)))
