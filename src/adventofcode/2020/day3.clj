(ns adventofcode.2020.day3)
(require '[adventofcode.util :as u])

(defn expand-field
  "Double the field to the right"
  [field]
  (vec (map #(vec (concat % %)) field)))

(defn count-symbols
  ([field symbols slope coords counts]
   (let [max-y (count field) max-x (count (first field))
         x (:x coords) y (:y coords)
         new-x (+ x (:x slope)) new-y (+ y (:y slope))
         new-coords {:x new-x :y new-y}
         tree (:tree symbols)
         tree-count (:tree counts) open-count (:open counts)]
     (cond
       ;; Reached the bottom of the field
       (>= y max-y) counts
       ;; Reached the right edge, so double field and try again
       (>= x max-x) (count-symbols (expand-field field) symbols slope coords counts)
       ;; Keep going down
       :else (let [thing (u/find-in-matrix field coords)]
               (if (= tree thing)
                 (count-symbols field symbols slope new-coords {:tree (inc tree-count) :open open-count})
                 (count-symbols field symbols slope new-coords {:tree tree-count :open (inc open-count)}))))))
  ([field symbols slope]
   (count-symbols field symbols slope {:x 0 :y 0} {:tree 0 :open 0})))

(defn solution []
  (let [day3-data (u/read-file "./data/day/3/input.txt")
        field (u/parse-into-matrix day3-data)
        slopes [{:x 1 :y 1} {:x 3 :y 1} {:x 5 :y 1} {:x 7 :y 1} {:x 1 :y 2}]
        symbols {:tree \# :open \.}]
    {:part1 (count-symbols field symbols {:x 3 :y 1})
     :part2 (reduce * (map #(:tree (count-symbols field symbols %)) slopes))}))
