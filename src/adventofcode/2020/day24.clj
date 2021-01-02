(ns adventofcode.2020.day24)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def HALF (/ 1 2))
(def DIRECTIONS
  {:e  (list 1 0)
   :se (list HALF -1)
   :sw (list (- HALF) -1)
   :w  (list -1 0)
   :nw (list (- HALF) 1)
   :ne (list HALF 1)})

(defn travel
  [line-str]
  (loop [cur line-str
         pos '(0 0)
         prev nil]
    (let [head (first cur)
          tail (rest cur)]
      ; (printf "cur: %s pos: %s prev: %s\n" cur (vec pos) prev)
      (cond
        (empty? cur) pos
        (or (= head \s) (= head \n)) (recur tail pos head)
        ; for e / w, combine with last and apply the direction
        :else (let [dir-name (keyword (str prev head))
                    direction (DIRECTIONS dir-name)]
                (recur tail (u/zip-and-reduce + pos direction) nil))))))

(defn- flip-tile
  [state]
  (if (= state :w) :b :w))

(defn- flip-tile-in-map
  [map tile]
  (update-in map [tile] (fnil flip-tile :w)))

(defn renovate
  [lines]
  (reduce flip-tile-in-map {} (map travel lines)))

; part 2

(defn find-neighbors
  [tile]
  (map #(u/zip-and-reduce + % tile) (vals DIRECTIONS)))

(defn apply-rules-to-tile
  [tiles tile neighbors]
  (let [state (tiles tile)
        neighbor-states (map tiles neighbors)
        black-neighbors (filter #(= % :b) neighbor-states)
        black-count (count black-neighbors)]
    (cond
      (= state :b) (if (or (= 0 black-count) (> black-count 2)) :w :b)
      :else (if (= 2 black-count) :b :w))))

(defn apply-rules-to-all-tiles
  [tiles neighbors-fn]
  (loop [cur-tiles tiles
         result-tiles {}]
    (if (empty? cur-tiles)
      result-tiles
      (let [head (first cur-tiles)                          ; key-value pair
            head-tile (first head)
            neighbors (neighbors-fn head-tile)]
        (recur
          (rest cur-tiles)
          (assoc result-tiles head-tile (apply-rules-to-tile tiles head-tile neighbors)))))))

(defn count-tiles
  [tiles state-req]
  (count (filter (fn [[_ state]] (= state state-req)) tiles)))

(defn run-art-exhibit
  [tiles num-days]
  (let [find-neighbors-fn (memoize find-neighbors)]
    (loop [day num-days
           cur-tiles tiles]
      ; (printf "day: %s count: %s\n" day (count-tiles cur-tiles :b))
      (if (= day 0)
        cur-tiles
        (let [all-tiles (keys cur-tiles)
              all-neighbors (set (apply concat (map find-neighbors-fn all-tiles)))
              with-neighbors-added (u/update-vals cur-tiles all-neighbors (fnil identity :w))]
          ;(printf "all-tiles: %s\nall-neighbors: %s\nwith-neighbors-added: %s\n\n"
          ;        all-tiles all-neighbors with-neighbors-added)
          (recur (dec day) (apply-rules-to-all-tiles with-neighbors-added find-neighbors-fn)))))))


(def data (u/read-file "./data/day/24/input.txt"))
(def data-small (u/read-file "./data/day/24/input-small.txt"))

(defn solve
  ([data]
   (let [flipped (renovate data)]
     {:part1 (count-tiles flipped :b)
      :part2 (count-tiles (run-art-exhibit flipped 100) :b)}))
  ([]
   (solve data)))