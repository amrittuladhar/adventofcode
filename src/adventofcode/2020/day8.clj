(ns adventofcode.2020.day8)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn run-recur
  [instructions index acc seen-indexes]
  (if (= index (count instructions))
    {:acc acc :terminated true}
    (let [current-instruction (nth instructions index)
          op (:command current-instruction)
          value (:value current-instruction)]
      (cond
        (contains? seen-indexes index) {:acc acc :terminated false}
        (= op :nop) (run-recur instructions (inc index) acc (conj seen-indexes index))
        (= op :acc) (run-recur instructions (inc index) (+ acc value) (conj seen-indexes index))
        ; jmp
        :else (run-recur instructions (+ index value) acc (conj seen-indexes index))))))

(defn run
  [instructions start-index start-acc]
  (run-recur instructions start-index start-acc #{}))

(defn swap-instruction
  "instructions has to be a vector"
  [instructions index]
  (let [instruction (nth instructions index)]
    (cond
      (= :jmp (:command instruction)) (assoc instructions index {:command :nop :value (:value instruction)})
      (= :nop (:command instruction)) (assoc instructions index {:command :jmp :value (:value instruction)})
      :else nil)))

(defn fix
  [instructions index]
  (if (= index (count instructions))
    {:acc -1 :terminated false}
    (let [swapped (swap-instruction instructions index)]
      (if (nil? swapped)
        (fix instructions (inc index))
        (let [run-after-swap (run swapped 0 0)]
          (if (:terminated run-after-swap)
            run-after-swap
            (fix instructions (inc index))))))))

(defn parse-instruction
  [line]
  (let [tokens (str/split line #" ")
        command (first tokens)
        steps (u/to-int (second tokens))]
    {:command (keyword command) :value steps}))

(def data (u/read-file "./data/day/8/input.txt"))

(defn solution []
  {:part1 (:acc (run (map parse-instruction data) 0 0))
   :part2 (fix (vec (map parse-instruction data)) 0)})