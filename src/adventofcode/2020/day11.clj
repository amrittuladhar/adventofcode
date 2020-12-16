(ns adventofcode.2020.day11)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(defn parse
  [lines]
  (u/parse-into-matrix lines #(keyword (str %))))

(def data (parse (u/read-file "./data/day/11/input.txt")))

(def data-small (parse
                  (str/split "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL" #"\n")))

(def data-small-2
  (parse
    (str/split ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#....." #"\n")))

(defn first-seat-by-direction
  [matrix coords direction]
  (let [matrix-size (u/matrix-size matrix)
        matrix-x (:x matrix-size) matrix-y (:y matrix-size)
        x (:x coords) y (:y coords)
        next-coords (u/zip-and-reduce + (list x y) direction)
        next-x (first next-coords) next-y (second next-coords)]
    ; (printf "x: %s y: %s next-coords: %s next-x: %s next-y: %s\n" x y (vec next-coords) next-x next-y)
    (cond
      (< next-x 0) nil
      (< next-y 0) nil
      (>= next-x matrix-x) nil
      (>= next-y matrix-y) nil
      :else (let [current-value (u/find-in-matrix matrix {:x next-x :y next-y})]
              (if (= current-value :.)
                (first-seat-by-direction matrix {:x next-x :y next-y} direction) ; floor, so keep going
                {:x next-x :y next-y})))))

(defn first-seats
  ([matrix coords]
   (let [directions '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 0) (1 -1) (1 1))]
     (map #(list (:x %) (:y %)) (filter #(not (nil? %)) (map #(first-seat-by-direction matrix coords %) directions))))))

(def rules
  [{:seat                  :L
    :adjacent-finder       #(u/adjacent-in-matrix (u/matrix-size %1) %2)
    :adjacent-count-filter #(= % 0)
    :adjacent-filter       #(= % :#)
    :evolve-to             :#}
   {:seat                  :#
    :adjacent-finder       #(u/adjacent-in-matrix (u/matrix-size %1) %2)
    :adjacent-count-filter #(>= % 4)
    :adjacent-filter       #(= % :#)
    :evolve-to             :L}])

(def rules-part-2
  [{:seat                  :L
    :adjacent-finder       first-seats
    :adjacent-count-filter #(= % 0)
    :adjacent-filter       #(= % :#)
    :evolve-to             :#}
   {:seat                  :#
    :adjacent-finder       first-seats
    :adjacent-count-filter #(>= % 5)
    :adjacent-filter       #(= % :#)
    :evolve-to             :L}])

(defn apply-rule
  "Applies the given rule to the given co-ordinates and returns the new value of the seat, or nil if unchanged"
  [matrix coords rule]
  (let [current-seat (u/find-in-matrix matrix coords)
        adjacent-finder (:adjacent-finder rule)
        adjacent-coords (adjacent-finder matrix coords)
        adjacent-seats (map #(u/find-in-matrix matrix {:x (first %) :y (second %)}) adjacent-coords)]
    ; (printf "current: %s adjacent coords: %s adjacent seats: %s\n" current-seat (vec adjacent-coords) (vec adjacent-seats))
    (if (and
          (= (:seat rule) current-seat)
          ((:adjacent-count-filter rule) (count (filter (:adjacent-filter rule) adjacent-seats))))
      (:evolve-to rule)
      nil)))

(defn evolve-seat
  "Applies all given rules to given co-ordinates and returns the new value of the seat"
  [matrix rules coords]
  (loop [current-seat (u/find-in-matrix matrix coords)
         current-rules rules]
    (if (empty? current-rules)
      current-seat
      (let [applied (apply-rule matrix coords (first current-rules))
            new-value (if (nil? applied) current-seat applied)]
        (recur new-value (rest current-rules))))))


(defn evolve [matrix rules]
  (let [max-y (dec (count matrix))
        max-x (dec (count (first matrix)))]
    (loop [current-x 0
           current-y 0
           current-matrix matrix]
      ; (printf "cur-x: %s cur-y: %x matrix: %s\n" current-x current-y current-matrix)
      (cond
        (> current-x max-x) (recur 0 (inc current-y) current-matrix)
        (> current-y max-y) current-matrix
        :else (let [coords {:x current-x :y current-y}
                    new-seat (evolve-seat matrix rules coords)]
                ; (printf "x: %x y: %s new-seat: %s\n" current-x current-y new-seat)
                (recur (inc current-x) current-y (u/update-in-matrix current-matrix coords new-seat)))))))

(defn evolve-until-stable
  ([matrix rules max-rounds]
   (loop [last-matrix matrix
          new-matrix (evolve matrix rules)
          count max-rounds]
     (cond
       (= last-matrix new-matrix) last-matrix
       (= count 0) nil
       :else (recur new-matrix (evolve new-matrix rules) (dec count)))))
  ([matrix rules]
   (evolve-until-stable matrix rules 1000)))

(defn count-occupied
  [matrix]
  (u/count-in-matrix matrix #(= % :#)))

(defn solution []
  {:part1 (count-occupied (evolve-until-stable data rules))
   :part2 (count-occupied (evolve-until-stable data rules-part-2))})