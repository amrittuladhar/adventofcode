(ns adventofcode.2020.day13)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(defn parse-buses
  [line]
  (let [tokens (str/split line #",")]
    (map #(if (= "x" %) "x" (u/to-int %)) tokens)))

(defn parse
  [lines]
  {:timestamp (u/to-int (first lines))
   :bus-ids   (parse-buses (second lines))})

(def data (parse (u/read-file "./data/day/13/input.txt")))

(def data-small
  (list (parse-buses "17,x,13,19")
        (parse-buses "67,7,59,61")
        (parse-buses "67,x,7,59,61")
        (parse-buses "67,7,x,59,61")
        (parse-buses "1789,37,47,1889")))

(defn find-matching-buses
  [{ts :timestamp bus-ids :bus-ids}]
  (let [buses (filter int? bus-ids)]
    (loop [current-ts ts]
      (let [matching-buses (filter #(= (mod current-ts %) 0) buses)]
        ; (printf "ts: %s buses: %s matching: %s\n" ts (vec buses) (vec matching-buses))
        (if (not (empty? matching-buses))
          {:timestamp current-ts :bus (first matching-buses)}
          (recur (inc current-ts)))))))

; Chinese remainder theorem!
; https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
; (assume all the numbers are mutually co-prime)
; If bus IDs are m1, m2, x, x, m3, m4 and the answer is n
; we basically have to solve
;     n = 0 (mod m1)
;     n + 1 = 0 (mod m2) => n = -1 (mod m2)
;     <skip 2>
;     <skip 3>
;     n + 4 = 0 (mod m2) => n = -4 (mod m3)
;     n + 5 = 0 (mod m4) => n = -5 (mod m4)

(defn calculate-term
  [remainder M mi]
  (if (not (int? mi))
    0                                                       ; skip "x"s
    (let [bi (/ M mi)
          bi-prime (u/modular-inverse bi mi)]
      ; (printf "M: %s remainder: %s m: %s bi: %s bi-prime %s\n" M remainder mi bi bi-prime)
      ; (printf "inverse? %s\n" (mod (* bi bi-prime) m))
      (* remainder (mod (*' bi bi-prime) M)))))

(defn find-earliest-timestamp
  [bus-ids]
  (let [buses (filter int? bus-ids)
        M (reduce * buses)
        terms (map-indexed (fn [index m] (calculate-term (- index) M m)) bus-ids)]
    ; (printf "buses: %s M: %s\n terms: %s\n" (vec buses) M (vec terms))
    (mod (reduce + terms) M)))

(defn solution
  ([input]
   {:part1 (let [matching-bus (find-matching-buses input)
                 timestamp (:timestamp matching-bus)
                 bus (:bus matching-bus)
                 wait-time (- timestamp (:timestamp input))]
             (* wait-time bus))
    :part2 (find-earliest-timestamp (:bus-ids input))})
  ([] (solution data)))

; 190510384131077010 - incorrect
; 894834707918021 - answer too high