(ns adventofcode.2021.day9)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn get-low-points [matrix]
  ; (println matrix)
  (let [size (u/matrix-size matrix)
        x-max (range (:x size))
        y-max (range (:y size))]
    (for [x x-max y y-max
          :let [v (u/find-in-matrix matrix x y)
                neighbor-coords (u/adjacent-in-matrix-no-diagonals (dec (:x size)) (dec (:y size)) x y)
                neighbors (map #(u/find-in-matrix matrix (first %) (second %)) neighbor-coords)]
          :when (every? #(> % v) neighbors)]
      {:coords [x y] :value v})))

(defn value-with-coords [matrix [x y]]
  {
   :coords [x y]
   :value  (u/find-in-matrix matrix x y)
   })

(defn larger-neighbor-filter-fn [value neighbor]
  (let [neighbor-value (:value neighbor)]
    (and (> neighbor-value value) (not (= neighbor-value 9)))))

; for a given low point, calculate the size of the basin
(defn get-basin-size [matrix low-point basin-set]
  ; (println "set: " basin-set)
  (let [size (u/matrix-size matrix)
        check-value (:value low-point)
        check-x (first (:coords low-point))
        check-y (second (:coords low-point))]
    ; (println "   checking " check-x check-y "value: " check-value)
    (let [neighbor-coords (u/adjacent-in-matrix-no-diagonals (dec (:x size)) (dec (:y size)) check-x check-y)
          neighbors (map #(value-with-coords matrix %) neighbor-coords)
          ; basin is formed by any neighbor that's larger than me but not a nine
          larger-neighbors (filter #(larger-neighbor-filter-fn check-value %) neighbors)]
      ; (println "   larger-neighbors " larger-neighbors)
      (cond
        (empty? larger-neighbors) basin-set
        :else (set (apply concat (map #(get-basin-size matrix % (conj basin-set %)) larger-neighbors)))))))


(defn solution []
  (let [data (u/read-file "./data/2021/day9/input.txt")
        lines (map #(str/split % #"") data)
        heights (u/parse-into-matrix lines u/to-int)
        low-points (get-low-points heights)]
    {
     :part1 (->> low-points
                 (map :value)
                 (map inc)
                 (reduce +))
     :part2 (let [all-basins (map #(get-basin-size heights % #{%}) low-points)
                  sorted-by-size (sort-by #(- (count %)) all-basins)
                  first-three (map #(nth sorted-by-size %) '(0 1 2))]
              (reduce * (map count first-three)))
     }))
