(ns adventofcode.2021.day17)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn towards-zero [v]
  (cond
    (= v 0) v
    (> v 0) (- v 1)
    :else (+ v 1)))

(defn update-position [[x y] [vx vy]]
  ; (println "x:" x "y:" y "vx:" vx "vy:" vy)
  [[(+ x vx)
    (+ y vy)]
   [(towards-zero vx)
    (- vy 1)]])

(defn in-target? [{[min-x max-x] :x
                   [min-y max-y] :y}
                  [x y]]
  ; (println "x" x "y" y "min-x" min-x "max-x" max-x "min-y" min-y "max-y" max-y)
  (and
    (>= x min-x) (<= x max-x)
    (>= y min-y) (<= y max-y)))

(defn beyond-target? [{[_ max-x] :x
                       [min-y _] :y}
                      [x y]]
  ; (println "beyond?" "x" x "max-x" max-x "y" y "min-y" min-y)
  (or (> x max-x) (< y min-y)))

(defn find-trajectory [target
                  [ix iy]
                  [ivx ivy]]
  (loop [[x y] [ix iy]
         [vx vy] [ivx ivy]
         path '()
         path-set #{}]
    ; (println "x" x "y" y "vx" vx "vy" vy "target" target "path" path)
    (cond
      (in-target? target [x y]) {:iv [ivx ivy] :path (conj path [x y])}
      ; (contains? path-set [x y]) nil
      (beyond-target? target [x y]) nil
      :else (let [[[new-x new-y] [new-vx new-vy]] (update-position [x y] [vx vy])]
              (recur [new-x new-y]
                     [new-vx new-vy]
                     (conj path [x y])
                     (conj path-set [x y]))))))

(defn find-possible-trajectories [target candidates-fn]
  (let [candidates (candidates-fn target)]
    ; (println "candidates" candidates)
    (filter
      #(not (nil? %))
      (map
        #(find-trajectory target [0 0] [(first %) (second %)])
        candidates))))

(defn find-max-y [trajectories]
  (apply max
         (map
           (fn [trajectory] (apply max (map second (:path trajectory))))
           trajectories)))

(defn parse-target-area [definition]
  (let [tokens (str/split definition #"=")
        area (str/split (tokens 1) #"\.\.")]
    [
     (u/to-int (area 0))                                    ; min
     (u/to-int (area 1))                                    ; max
     ]))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day17/" file ".txt"))
        line (first lines)
        target-area (.substring line 14)
        tokens (str/split target-area #", ")]
    {
     :x (parse-target-area (tokens 0))
     :y (parse-target-area (tokens 1))
     }
    ))

(defn candidate-velocities [{[_ max-x] :x [min-y _] :y}]
  ; I thought this would need more thought to narrow the search space, but the following seems to just work
  ; ... and it didn't take too long to compute the answer
  (for [x (range 1 (inc max-x)) y (range min-y (inc (- min-y)))] [x y]))

(defn solution [file]
  (let [target (parse file)
        trajectories (find-possible-trajectories target candidate-velocities)]
    {
     :max-height (find-max-y trajectories)
     :count (count trajectories)
     }))

