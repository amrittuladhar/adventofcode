(ns adventofcode.2020.day17)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def rules
  [{:state                 "#"
    :neighbor-count-filter #(not (or (= % 2) (= % 3)))
    :neighbor-filter       #(= % "#")
    :evolve-to             "."}
   {:state                 "."
    :neighbor-count-filter #(= % 3)
    :neighbor-filter       #(= % "#")
    :evolve-to             "#"}])

(defn project-matrix
  [matrix & projections]
  ; (printf "projections: %s\n" projections)
  (u/filter-keys matrix (fn [[_ _ & zs]] (= zs projections))))

(defn print-matrix
  [matrix & ws]
  ; (printf "print-matrix: ws: %s\n" ws)
  (let [projected (apply project-matrix (cons matrix ws))
        sorted (sort-by (fn [[[x y & _] _]] (str x y)) projected)]
    (loop [remaining sorted
           current-x (first (first (first sorted)))
           prev-x current-x
           build-string ""]
      (cond
        (empty? remaining) build-string
        :else (let [value (second (first remaining))
                    tail (rest remaining)
                    next-x (first (first (first tail)))]
                ;(printf "value: %s prev-x: %s current-x: %s next-x: %s tail: %s \n"
                ;      value prev-x current-x next-x tail)
                (recur
                  tail
                  next-x
                  current-x
                  (str build-string
                       (if (= current-x prev-x) "" "\n")
                       (str/join value))))))))

(defn parse
  ([lines num-dimensions data x]
   (if (empty? lines)
     data
     (let [head (first lines)
           tokens (str/split head #"")
           tokens-indexed (map-indexed #(vector %1 %2) tokens)
           zeros (repeat (- num-dimensions 2) 0)
           data-update-pairs (map (fn [[y value]] (vector (concat (list x y) zeros) value)) tokens-indexed)
           data-update (u/pairs-to-map data-update-pairs)
           tail (rest lines)]
       (parse tail num-dimensions (merge data data-update) (inc x)))))
  ([lines num-dimensions]
   (parse lines num-dimensions {} 0)))

;; adapted from day11 solution

(defn apply-rule
  "Applies the given rule to the given co-ordinates and returns the new value of the seat, or nil if unchanged"
  [matrix coords rule]
  (let [current-state (matrix coords)
        neighbor-coords (u/find-neighbors coords)
        neighbor-values (map matrix neighbor-coords)
        neighbor-count-filter (:neighbor-count-filter rule)
        neighbor-filter (:neighbor-filter rule)]
    ;(print "  state: " current-state "\n  coords: " coords "\n  neighbor-values: " (filter #(not (nil? %)) neighbor-values) "\n\n")
    (if (and
          (= (:state rule) current-state)
          (neighbor-count-filter
            (count (filter neighbor-filter neighbor-values))))
      (:evolve-to rule)
      nil)))

(defn apply-rules
  [matrix coords rules]
  (let [applied
        (first (for [rule rules
                     :let [new-state (apply-rule matrix coords rule)]
                     :when (not (nil? new-state))]
                 new-state))]
    (if (nil? applied)
      (matrix coords)
      applied)))

(defn add-neighbors
  [matrix value-if-absent]
  ;(printf "matrix: %s value: %s\n" matrix value-if-absent)
  (loop [loop-matrix matrix
         updated-matrix matrix]
    (if (empty? loop-matrix)
      updated-matrix
      (recur
        (rest loop-matrix)
        ; for each coords, find all neighbors, and if we don't already have states for them
        ; set the state to value-if-absent
        (let [neighbors (u/find-neighbors (first (first loop-matrix)))]
          ;(printf "coords: %s neighbors: %s\n" (first (first loop-matrix)) (vec neighbors))
          (u/update-vals updated-matrix neighbors #(u/if-nil? % value-if-absent)))))))

(defn evolve
  [matrix rules]
  ; evolving may change all neighbors of existing cells, so add them to the matrix
  (let [matrix-with-neighbors (add-neighbors matrix ".")]
    (loop [updated-matrix matrix-with-neighbors
           loop-matrix matrix-with-neighbors]
      (if (empty? loop-matrix)
        updated-matrix
        (let [head (first loop-matrix)
              head-coords (first head)
              updated-state (apply-rules matrix-with-neighbors head-coords rules)]
          ;(printf "head: %s coords: %s updated: %s\n" head (vec head-coords) updated-state)
          (recur
            (assoc updated-matrix head-coords updated-state)
            (rest loop-matrix)))))))

(def data-small (parse (u/read-file "./data/day/17/input-small.txt") 3))
(def data (parse (u/read-file "./data/day/17/input.txt") 3))

(def data-small-4d (parse (u/read-file "./data/day/17/input-small.txt") 4))
(def data-4d (parse (u/read-file "./data/day/17/input.txt") 4))

(defn count-active
  [matrix]
  (count (filter (fn [[_ val]] (= val "#")) matrix)))

(defn solve
  ([data]
   (count-active (nth (iterate #(evolve % rules) data) 6)))
  ([]
   {:part1 (solve data)
    :part2 (solve data-4d)}))