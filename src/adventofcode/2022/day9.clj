(ns adventofcode.2022.day9)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

; Quite a bit of cruft in here to be cleaned up later

(def MOVES
  {
   :R [1 0]
   :L [-1 0]
   :U [0 1]
   :D [0 -1]
   })

(defn parse-line [line]
  (let [tokens (str/split line #" ")]
    {:direction (keyword (first tokens))
     :magnitude (u/to-int (second tokens))}))

(defn print-position [{[head-x head-y] :head [tail-x tail-y] :tail}]
  (-> (merge
        {[0 5] "s"}
        {[head-x (- 5 head-y)] "H"}
        {[tail-x (- 5 tail-y)] "T"})
      (u/convert-matrix-map-to-vec ".")
      (u/print-matrix)))

(defn print-positions [positions]
  (-> (merge
        (->> (map :head positions)
             (map #(vector % "H"))
             u/pairs-to-map)
        (->> (map :tail positions)
             (map #(vector % "T"))
             u/pairs-to-map))
      (doto prn)))
; (u/convert-matrix-map-to-vec ".")
; u/print-matrix))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day9/" file ".txt"))]
    (map parse-line lines)))

(defn adjacent? [x1 y1 x2 y2]
  ; (prn "adjacent?" x1 y1 x2 y2)
  (-> (and
        (<= (Math/abs (- x1 x2)) 1)
        (<= (Math/abs (- y1 y2)) 1))))

(defn tail-change [diff-from-head]
  (cond
    (= diff-from-head 0) 0
    (>= diff-from-head 1) 1
    (<= diff-from-head 1) -1))

(defn move-tail [head-x head-y tail-x tail-y]
  (let [diff-x (- head-x tail-x)
        diff-y (- head-y tail-y)
        x-change (tail-change diff-x)
        y-change (tail-change diff-y)]
    ; (println "head:" head-x head-y "tail:" tail-x tail-y "diff:" diff-x diff-y "change:" x-change y-change)
    (cond
      (adjacent? head-x head-y tail-x tail-y) [tail-x tail-y]
      :else [(+ tail-x x-change)
             (+ tail-y y-change)])))


(defn apply-move [move position]
  (let [direction (:direction move)
        [change-x change-y] (MOVES direction)
        magnitude (:magnitude move)]
    (prn move)
    ; (println "move:" move "position:" position "magnitude:" magnitude "change-x:" change-x "change-y:" change-y)
    (->> (reduce
           (fn [[accumulator last-position] _]
             ; (println "acc:" accumulator "last:" last-position)
             (let [{[last-head-x last-head-y] :head
                    [last-tail-x last-tail-y] :tail} last-position
                   new-head-x (+ change-x last-head-x)
                   new-head-y (+ change-y last-head-y)
                   [new-tail-x new-tail-y] (move-tail new-head-x new-head-y last-tail-x last-tail-y)
                   new-position {:head [new-head-x new-head-y]
                                 :tail [new-tail-x new-tail-y]}]
               ; (println "new:" new-position)
               ; (print-position new-position)
               (prn)
               ; (print-positions accumulator)
               [(conj accumulator new-position) new-position]))
           ['() position]
           (range 1 (inc magnitude)))
         first)))

(defn move [moves]
  (let [initial {:head [0 0] :tail [0 0]}]
    (reduce
      (fn [accumulator move]
        (let [new-positions (apply-move move (first accumulator))]
          (concat new-positions accumulator)))
      (list initial)
      moves)))
;
(defn part1 [file]
  (let [moves (parse file)]
    (->> (move moves)
         (map :tail)
         set
         count)))

; part2
(def KNOTS (->> (range 0 10)
                (map str)
                (map keyword)))

(defn new-rope-position [position]
  (-> (reduce
        (fn [[accumulator last-knot] current-knot]
          (let [[last-knot-x last-knot-y] (accumulator last-knot)
                [current-knot-x current-knot-y] (accumulator current-knot)]
            [
             (merge accumulator {current-knot (move-tail last-knot-x last-knot-y current-knot-x current-knot-y)})
             current-knot
             ]))
        [position :0]
        (rest KNOTS))
      first))

(defn apply-move-part2 [move position]
  (let [direction (:direction move)
        [change-x change-y] (MOVES direction)
        magnitude (:magnitude move)]
    ; (prn "apply-part2" "position" position)
    (->> (reduce
           (fn [[accumulator last-position] _]
             (prn "apply-part2" "last" last-position)
             (let [[last-head-x last-head-y] (last-position :0)
                   new-head-x (+ change-x last-head-x)
                   new-head-y (+ change-y last-head-y)
                   new-head {:0 [new-head-x new-head-y]}
                   new-position (new-rope-position (merge last-position new-head))]
               [(conj accumulator new-position) new-position]))
           ['() position]
           (range 1 (inc magnitude)))
         first)))

(defn move-part2 [moves]
  (let [initial (u/pairs-to-map (map #(vector % [0 0]) KNOTS))]
    (prn "move-part2" "initial" initial)
    (reduce
      (fn [accumulator move]
        (let [new-positions (apply-move-part2 move (first accumulator))]
          (concat new-positions accumulator)))
      (list initial)
      moves)))

(defn part2 [file]
  (let [moves (parse file)]
    (->> (move-part2 moves)
         (map :9)
         set
         count)))
