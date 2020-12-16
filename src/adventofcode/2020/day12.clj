(ns adventofcode.2020.day12)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(defn move
  [[x y] [move-x move-y]]
  (list (+ move-x x) (+ move-y y)))

(def DIRECTIONS
  {:north {:angle 90 :op :N}
   :south {:angle 270 :op :S}
   :east {:angle 0 :op :E}
   :west {:angle 180 :op :W}})

(defn turn
  [directions-map current-dir angle]
  (mod (+ (:angle (directions-map current-dir)) angle) 360))

(defn direction-by-angle
  [angle]
  (first (map first (filter (fn [[key val]] (= (:angle val) angle)) DIRECTIONS))))

(def OPERATIONS
  {:N (fn [steps {pos :pos}] {:pos (move pos (list 0 steps))})
   :S (fn [steps {pos :pos}] {:pos (move pos (list 0 (- steps)))})
   :E (fn [steps {pos :pos}] {:pos (move pos (list steps 0))})
   :W (fn [steps {pos :pos}] {:pos (move pos (list (- steps) 0))})
   ; to move forward, find current direction, which gives the corresponding operation to use to update position
   :F (fn [steps {dir :dir pos :pos}] ((-> dir DIRECTIONS :op OPERATIONS) steps {:pos pos :dir dir}))
   :L (fn [degrees {dir :dir}] {:dir (direction-by-angle (turn DIRECTIONS dir degrees))})
   :R (fn [degrees {dir :dir}] {:dir (direction-by-angle (turn DIRECTIONS dir (- 360 degrees)))})})

(defn update-waypoint-pos
  [operations-map operation num {{pos :pos} :waypoint}]
  {:waypoint ((operations-map operation) num {:pos pos})})

(defn rotate-waypoint-pos
  [degrees {{waypoint-pos :pos} :waypoint {ship-pos :pos} :ship}]
  (let [rotated (u/relative-to-new-origin waypoint-pos ship-pos #(u/rotate-around-origin degrees %))]
    {:waypoint {:pos rotated}}))

(def OPERATIONS-2
  {:N (fn [steps state] (update-waypoint-pos OPERATIONS :N steps state))
   :S (fn [steps state] (update-waypoint-pos OPERATIONS :S steps state))
   :E (fn [steps state] (update-waypoint-pos OPERATIONS :E steps state))
   :W (fn [steps state] (update-waypoint-pos OPERATIONS :W steps state))

   :L (fn [degrees state] (rotate-waypoint-pos degrees state))
   :R (fn [degrees state] (rotate-waypoint-pos (- degrees) state))

   ; F means we move the ship to where the waypoint is "n" times, but every time the ship
   ; moves, the waypoint also moves relative to where the ship is
   :F (fn [num
           {{waypoint-pos :pos} :waypoint
            {ship-pos :pos} :ship}]
        (let [waypoint-pos-diff (u/zip-and-reduce - waypoint-pos ship-pos)]
          ; (printf "waypoint-pos-diff: %s\n" (vec waypoint-pos-diff))
          (loop [count num
                 current-waypoint-pos waypoint-pos
                 current-ship-pos ship-pos]
            ; (printf "  count: %s waypoint-pos: %s ship-pos: %s\n" count (vec current-waypoint-pos) (vec current-ship-pos))
            (if (= count 0)
              {:waypoint {:pos current-waypoint-pos}
               :ship {:pos current-ship-pos}}
              (let [next-ship-pos current-waypoint-pos
                    next-waypoint-pos (u/zip-and-reduce + next-ship-pos waypoint-pos-diff)
                    next-count (dec count)]
                ; (printf "  NEXT: count: %s waypoint-pos: %s ship-pos: %s\n" next-count (vec next-waypoint-pos) (vec next-ship-pos))
                (recur next-count next-waypoint-pos next-ship-pos))))))})

; Answers obtained using OPERATIONS-2 is not being accepted so trying a different approach

(defn update-waypoint-pos-3
  [operations-map operation steps {pos :waypoint}]
  {:waypoint (:pos ((operations-map operation) steps {:pos pos}))})

(defn rotate-waypoint-pos-3
  [degrees {waypoint-pos :waypoint ship-pos :ship}]
  {:waypoint (u/rotate-around degrees waypoint-pos ship-pos) :ship ship-pos})

(def OPERATIONS-3
  (merge
    (reduce merge (map #(assoc {} % (fn [steps state] (update-waypoint-pos-3 OPERATIONS % steps state))) '(:N :S :E :W)))
    {
     ;:L (fn [degrees state] (rotate-waypoint-pos-3 degrees state))
     ;:R (fn [degrees state] (rotate-waypoint-pos-3 (- degrees) state))
     :L (fn [degrees state] (rotate-waypoint-pos-3 degrees state))
     :R (fn [degrees state] (rotate-waypoint-pos-3 (- degrees) state))

     :F (fn [steps {waypoint-pos :waypoint ship-pos :ship}]
          (let [diff (u/zip-and-reduce - waypoint-pos ship-pos)
                translate (map #(* steps %) diff)
                new-ship-pos (u/zip-and-reduce + translate ship-pos)
                new-waypoint-pos (u/zip-and-reduce + diff new-ship-pos)]
            (printf "oldship: %s oldw: %s diff: %s translate: %s newship: %s neww: %s\n"
                    ship-pos waypoint-pos
                    (vec diff)
                    (vec translate)
                    (vec new-ship-pos)
                    (vec new-waypoint-pos))
            {:ship new-ship-pos
             :waypoint new-waypoint-pos}))}))

; New approach - END

(defn parse
  [line]
  (let [matches (re-matches #"(.)(\d+)" line)]
    {:op (keyword (second matches)) :num (u/to-int (last matches))}))

(def data-small
  (map parse (str/split "F10\nN3\nF7\nR90\nF11" #"\n")))

(def data (map parse (u/read-file "./data/day/12/input.txt")))

(defn navigate
  [operations-map state operations-seq]
  (let [current-op (first operations-seq)
        op (:op current-op)
        operation-fn (operations-map op)]
    (printf "op: %s state: %s\n" current-op state)
    (if (empty? operations-seq)
      state
      (navigate operations-map (merge state (operation-fn (:num current-op) state)) (rest operations-seq)))))

(defn manhattan-distance
  [[^int x1 ^int y1] [^int x2 ^int y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn find-answer
  [operations-map initial pos-fn operations]
  (let [final (navigate operations-map initial operations)
        initial-pos (pos-fn initial)
        final-pos (pos-fn final)]
    {:initial initial-pos :final final-pos :distance (manhattan-distance (pos-fn initial) (pos-fn final))}))

(defn solution
  []
  {
   ;:part1 (find-answer OPERATIONS
   ;                    {:pos '(0 0) :dir :east} ; initial state
   ;                    :pos ; how to get the ship position from the state
   ;                    data)
   :part2 (find-answer OPERATIONS-2
                       {:ship {:pos '(0 0)} :waypoint {:pos '(10 1)}} ; initial state
                       #(:pos (:ship %)) ; how to get the ship position from the state
                       data)
   :part2-2 (find-answer OPERATIONS-3
                         {:ship '(0 0) :waypoint '(10 1)}
                         :ship
                         data)})
; 38294 - incorrect
; 46530 using second method - correct