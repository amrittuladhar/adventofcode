(ns adventofcode.2021.day15)
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.data.priority-map :refer [priority-map]])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day15/" file ".txt"))
        grid (u/parse-into-matrix-map
               (map (fn [line] (map u/to-int (str/split line #""))) lines))]
    grid))

(defn risk [grid position]
  (grid position))

(defn path-risk [grid path]
  ; (println path)
  (reduce + (map #(risk grid %) path)))

(defn neighbor-filter-fn [origin path]
  (fn [neighbor]
    (and
      (not (= origin neighbor))
      (not (contains? (set path) neighbor)))))

(defn sort-by-risk [grid positions]
  (sort-by #(- (risk grid %)) positions))

; Recursive with simple pruning - takes too long and blows the stack
(defn min-path [grid origin destination
                path
                min-so-far]
  (let [path-value (path-risk grid path)
        current (u/if-nil? (first path) origin)]
    ; (println path-value "(" min-so-far ") : current:" current);  ": " path)
    (cond
      (= current destination)
      (do
        ; (println "Reached " destination " with " (count path) ": " path-value "(" min-so-far ")")
        (if (< path-value min-so-far) path-value min-so-far))

      (> path-value min-so-far)
      (do
        ; (println "Pruning at " current)
        min-so-far)

      :else (let [max-y (second (u/matrix-map-dimensions grid)) max-x (first (u/matrix-map-dimensions grid))
                  neighbors (map #(apply vector %) (u/adjacent-in-matrix-no-diagonals max-x max-y (first current) (second current)))
                  to-visit (filter (neighbor-filter-fn origin path) neighbors)]
              ; (println "origin: " origin "current: " current "to-visit: " to-visit)
              (cond
                (empty? to-visit) min-so-far
                :else (reduce
                        (fn [m neighbor] (min-path
                                           grid
                                           origin
                                           destination
                                           (conj path neighbor)
                                           ; (+ path-value (u/find-in-matrix grid (first origin) (second origin)))
                                           m))
                        min-so-far
                        to-visit))))))

; Attempt 2
; From: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode

(defn find-neighbors [grid [x y]]
  (let [size (u/matrix-map-dimensions grid)
        max-x (first size) max-y (second size)]
    (map #(apply vector %) (u/adjacent-in-matrix-no-diagonals max-x max-y x y))))

; Thought it would be faster with a priority map for quicker lookups but still pretty slow
(defn find-candidate-better [grid to-visit risk-estimates]
  ; (println "to-visit" to-visit)
  ; risk-estimates is a priority map so keep taking from it until we find we haven't visited yet
  (loop [risk-estimates risk-estimates]
    (let [[position _] (first risk-estimates)]
      (cond
        (contains? to-visit position) (do
                                        ; (println "found" position)
                                        position)
        :else (recur (rest risk-estimates))))))

; Slow because it iterates through all unvisited nodes every time
(defn find-candidate [grid to-visit risk-estimates]
  ; (println to-visit distances)
  (reduce
    (fn [position next]
      (cond
        (< (risk-estimates position) (risk-estimates next)) position
        (= (risk-estimates position) (risk-estimates next)) (if (> (grid position) (grid next)) next position)
        :else next))
    to-visit))

(defn update-maps [grid candidate]
  (fn [[risk-estimates previous] neighbor]
    (let [neighbor-risk (grid neighbor)
          current-estimate (risk-estimates neighbor)
          new-estimate (+ (risk-estimates candidate) neighbor-risk)]
      ; (println "updating for " neighbor "current: " current-estimate "new: " new-estimate)
      ; (println "Assoc:" neighbor "->" candidate)
      (if (< new-estimate current-estimate)
        [(assoc risk-estimates neighbor new-estimate)
         (assoc previous neighbor candidate)]
        [risk-estimates previous]))))

(defn real-path [reverse-path destination]
  (reverse (take-while #(not (nil? %)) (iterate reverse-path destination))))

(defn djikstra-path [grid src destination]
  (let [all-positions (keys grid)]
    (loop [to-visit (set all-positions)
           reverse-path {}
           risk-estimates (into (priority-map) (map #(vector % (if (= % src) 0 Integer/MAX_VALUE)) all-positions))]
      (let [candidate (find-candidate-better grid to-visit risk-estimates)]
        ; (println "candidate:" candidate) ; "path map:" path-map)
        (cond
          (= candidate destination) reverse-path            ; todo
          :else (let [new-to-visit (disj to-visit candidate)
                      neighbors (filter #(contains? to-visit %) (find-neighbors grid candidate))
                      new-maps (reduce (update-maps grid candidate) [risk-estimates reverse-path] neighbors)]
                  (recur new-to-visit (second new-maps) (first new-maps))))))))


(defn calculate-min-risk [grid]
  (let [size (u/matrix-map-dimensions grid)
        max-x (first size) max-y (second size)
        src-risk (grid [0 0])]
    (- (reduce + (map
                   grid
                   (real-path (djikstra-path grid [0 0] [max-x max-y]) [max-x max-y]))) src-risk)))

(defn part1 [file]
  (calculate-min-risk (parse file)))

(defn wrap [value]
  (if (>= value 10) (inc (mod value 10)) value))

(defn expand-grid [grid times]
  (let [size (u/matrix-map-dimensions grid)
        x-size (inc (first size)) y-size (inc (second size))
        cells (for [x (range 0 times) y (range 0 times) :when (not (and (= x 0) (= y 0)))] [x y])]
    ; (println x-size y-size)
    (reduce
      (fn [iterate-grid [cx cy]]
        (merge iterate-grid
               (u/pairs-to-map
                 (map
                   (fn [[[x y] existing]]
                     (let [new-x (+ x (* cx x-size))
                           new-y (+ y (* cy y-size))
                           new-value (wrap (+ existing cx cy))]
                       ;(if (= [new-x new-y] [17 20])
                       ;  (println [cx cy] [x y] "->" [new-x new-y] existing "->" new-value))
                       ; (println "Adding Cell" [cx cy] existing [x y] new-value [new-x new-y])
                       (vector [new-x new-y] new-value)))
                   grid)
                 )))
      grid
      cells)))

(defn part2 [file]
  (calculate-min-risk (expand-grid (parse file) 5)))