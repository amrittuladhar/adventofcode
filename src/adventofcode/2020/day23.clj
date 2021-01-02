(ns adventofcode.2020.day23)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

; Attempt 1 using vectors - good enough for part 1

(defn partitionv
  [v start end]
  (let [diff (- (count v) end)]
    (cond
      (>= diff 0) {:prefix (take start v)
                   :sub    (subvec v start end)
                   :suffix (subvec v end)}
      :else (let [wrap (mod end (count v))]
              ; (printf "start: %s end: %s wrap: %s size: %s\n" start end wrap (count v))
              (if (> wrap start)
                (partitionv v start wrap)
                {:sub    (vec (concat (subvec v start) (subvec v 0 wrap)))
                 :suffix []
                 :prefix (subvec v wrap start)})))))

(defn find-destination
  [cups destination-label]
  (if (< destination-label (apply min cups))
    (.indexOf cups (apply max cups))
    (let [idx (.indexOf cups destination-label)]
      (if (= idx -1)
        (find-destination cups (dec destination-label))
        idx))))

(defn single-move
  [cupsv idx transfer-size]
  (let [current-cup (nth cupsv idx)
        transfer-start-idx (inc idx)
        partitioned (partitionv cupsv transfer-start-idx (+ transfer-start-idx transfer-size))
        move-cups (:sub partitioned)
        new-cupsv (vec (concat (:prefix partitioned) (:suffix partitioned)))]
    ;(printf "cups: %s[%s]\ncurrent: %s\nmoved: %s\nnew: %s[%s]\n"
    ;        (type cupsv) (count cupsv) current-cup (vec move-cups) (type new-cupsv) (count new-cupsv))
    (let [destination-cup-label (dec (u/to-int current-cup))
          destination-cup-pos (find-destination new-cupsv destination-cup-label)]
      ; (printf "destination: %s destination-pos: %s\n" destination-cup-label destination-cup-pos)
      (let [partition-idx (inc destination-cup-pos)
            partitioned (partitionv new-cupsv partition-idx partition-idx)
            prefix (:prefix partitioned)
            suffix (:suffix partitioned)
            final-cups (vec (concat prefix move-cups suffix))]
        ; (printf "final: %s\n\n" final-cups)
        final-cups))))

(defn find-current-index
  [cupsv last-label]
  (if (nil? last-label)
    0
    (let [idx (.indexOf cupsv last-label)]
      (if (= idx (dec (count cupsv)))
        0
        (inc idx)))))

(defn move
  [cupsv transfer-size iterations]
  (loop [current-cupsv cupsv
         last-label nil
         current-iterations iterations
         memo {}]
    (let [current-idx (find-current-index current-cupsv last-label)]
      ; (printf "last-label: %s\ncurrent-idx: %s\ncups: %s\n\n" last-label current-idx current-cupsv)
      (cond
        (= current-iterations 0) current-cupsv
        (= current-idx (count current-cupsv)) (recur current-cupsv 0 current-iterations memo)
        (contains? memo current-cupsv) (recur (memo current-cupsv) (nth current-cupsv current-idx) (dec current-iterations) memo)
        :else (let [result (single-move current-cupsv current-idx transfer-size)]
                (recur result (nth current-cupsv current-idx) (dec current-iterations) (assoc memo current-cupsv result)))))))

(defn parse-to-seq
  [cups-str]
  (map u/to-int (str/split cups-str #"")))

(defn parse
  [cups-str]
  (vec (parse-to-seq cups-str)))

(defn cupsv-to-str
  [cupsv]
  (let [idx (.indexOf cupsv 1)
        partitioned (partitionv cupsv idx (inc idx))
        result (concat (:suffix partitioned) (:prefix partitioned))]
    (str/join "" result)))

(defn solve-part-1
  [cups-str]
  (let [cupsv (parse cups-str)]
    (cupsv-to-str (move cupsv 3 100))))

; Second implementation using map to handle the larger part 2

(defn parse-to-map
  [cups-seq]
  (let [cup-pairs (u/partition-seq cups-seq 2)
        cups-map (u/pairs-to-map cup-pairs)
        cups-max (apply max cups-seq)]
    {:cups (assoc cups-map (last cups-seq) (first cups-seq))
     :max  cups-max}))

(defn find-destination-2
  [cups max-cup current picked-up]
  (let [next (dec current)]
    ;(printf "[find-destination-2] cups: %s max-cup: %s current: %s pick-up: %s\n"
    ;        (count cups) max-cup current picked-up)
    (cond
      (contains? picked-up next) (find-destination-2 cups max-cup next picked-up)
      (nil? (cups next)) (find-destination-2 cups max-cup (inc max-cup) picked-up)
      :else next)))

(defn find-move
  "returns map with :source :destination :pickup-start and :pickup-end"
  [cups max-cup current pickup-size]
  (let [next-to-current (cups current)
        pick-up (take pickup-size (iterate cups next-to-current))
        destination (find-destination-2 cups max-cup current (set pick-up))]
    ;(printf "current: %s\npick up: %s\ndestination: %s\n\n"
    ;        current (vec pick-up) destination)
    {:source current :destination destination :pickup-start (first pick-up) :pickup-end (last pick-up)}))

(defn pickup-and-move
  [cups source destination pickup-start pickup-end]
  (let [current-next-to-destination (cups destination)
        new-next-to-source (cups pickup-end)]
    (assoc cups source new-next-to-source destination pickup-start pickup-end current-next-to-destination)))

(defn move-2
  [cups max-cup start iterations pickup-size]
  (loop [current-cups cups
         current start
         current-iterations iterations]
    (if (= current-iterations 0)
      current-cups
    (let [move (find-move current-cups max-cup current pickup-size)
          source (:source move)
          destination (:destination move)
          pickup-start (:pickup-start move)
          pickup-end (:pickup-end move)
          new-cups (pickup-and-move current-cups source destination pickup-start pickup-end)]
      ;(printf "cups: %s\npick up: (%s %s)\ndestination: %s\n\n"
      ;        (count current-cups) pickup-start pickup-end destination)
      (recur new-cups (new-cups current) (dec current-iterations))))))

(def data-example (parse "389125467"))

(defn calculate-part-2
  [cups]
  (let [value-1 (cups 1)
        value-2 (cups value-1)]
    (* value-1 value-2)))

(defn solve-part-2
  [cups-str max-cups iterations]
  (let [prefix (parse cups-str)
        max-in-prefix (apply max prefix)
        all-cups (:cups (parse-to-map (concat prefix (range (inc max-in-prefix) (inc max-cups)))))]
    (calculate-part-2 (move-2 all-cups max-cups (first prefix) iterations 3))))
