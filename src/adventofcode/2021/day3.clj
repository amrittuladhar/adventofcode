(ns adventofcode.2021.day3)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn update-parity [[bit current-parity]]
  (if (= bit \0) (dec current-parity) (inc current-parity)))

; for each column in each line, finds a bit "parity" value (can't think of a better name)
; parity will be positive if there are more 1s and negative if there are more 0s
; e.g.
; 11100
; 00110
; 11100
; -> '(1 1 3 -1 -3)
(defn find-bit-parity
  ([input]                                                  ; if called with only "input", call again with "input" and the length of the first input
   (if (empty? input)
     nil
     (find-bit-parity input (count (first input)))))
  ([lines line-size]
   (reduce
     #(map update-parity (u/zip %2 %1))
     (repeat line-size 0)                                   ; initial parity - '(0 0 0 ... )
     lines)))

(defn find-rates
  [lines]
  (let [parity (find-bit-parity lines)]
    {
     :gamma   (str/join (map #(if (>= % 0) "1" "0") parity))
     :epsilon (str/join (map #(if (>= % 0) "0" "1") parity))
     }))

(defn filter-data [data parity-filter-fn]
  (loop [data data
         index 0]
    (let [parity-filter (parity-filter-fn data)]
      (if (= (count data) 1)
        (first data)
        (recur
          (filter #(= (nth % index) (nth parity-filter index)) data)
          (inc index))))))

(defn solution []
  (let [data (u/read-file "./data/2021/day3/input.txt")
        rates (find-rates data)]
    {
     :part1 (let [gamma (u/to-int 2 (:gamma rates))
                  epsilon (u/to-int 2 (:epsilon rates))]
              (* gamma epsilon))
     :part2 (let [o2-binary (filter-data data #(:gamma (find-rates %)))
                  co2-binary (filter-data data #(:epsilon (find-rates %)))
                  o2 (u/to-int 2 o2-binary)
                  co2 (u/to-int 2 co2-binary)]
              (* o2 co2))
     }))