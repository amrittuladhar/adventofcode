(ns adventofcode.2021.day11)
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn add-one [grid]
  (mapv
    (fn [row] (mapv inc row))
    grid))

(defn flashed? [item] (> item 9))

(defn flash [grid]
  (mapv
    (fn [row] (mapv #(if (flashed? %) 0 %) row))
    grid))

(defn flash-neighbors [grid already-flashed]
  (let [grid grid
        flashed-coords (filter
                         #(not (contains? already-flashed %))
                         (u/search-matrix grid flashed?))]
    ; (println "flashed: " flashed-coords)
    ; (println "already: " already-flashed)
    ; (u/print-matrix grid)
    (cond
      (empty? flashed-coords) (flash grid)
      :else (let [max-y (dec (count grid))
                  max-x (dec (count (first grid)))
                  all-affected-neighbors (filter
                                                #(not (contains? already-flashed %))
                                                (apply
                                                  concat
                                                  (map
                                                    (fn [[x y]] (u/adjacent-in-matrix max-x max-y x y))
                                                    flashed-coords)))
                  new-grid (u/update-in-matrix-fn grid inc all-affected-neighbors)]
              (flash-neighbors
                new-grid
                (set (concat flashed-coords already-flashed)))))))

(defn run-step [grid]
  (let [grid-1 (add-one grid)]
    grid (flash-neighbors grid-1 #{})))

(defn run-steps [grid steps]
  (loop [grid grid
         steps steps
         total-flashes 0]
    ; (println "remaining steps: " steps "total-flashed" total-flashes)
    (u/print-matrix grid)
    (cond
      (= steps 0) {:grid grid :total-flashes total-flashes}
      :else (let [grid (run-step grid)
                  flashes-count (u/count-in-matrix grid #(= % 0))]
              (recur grid (dec steps) (+ total-flashes flashes-count))))))

(defn all-flashing? [grid]
  (every? #(= % 0) (flatten grid)))

(defn find-all-flash [grid]
  (drop-while
    #(not (all-flashing? (:grid %)))
    (map-indexed
      (fn [index grid] {:index index :grid grid})
      (iterate run-step grid))))

(defn solution []
  (let [data (u/read-file "./data/2021/day11/input.txt")
        octopuses (u/parse-into-matrix data u/to-int)]
    {
     :part1 (:total-flashes (run-steps octopuses 100))
     :part2 (:index (first (find-all-flash octopuses)))
     }))
