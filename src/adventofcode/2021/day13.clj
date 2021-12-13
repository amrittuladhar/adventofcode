(ns adventofcode.2021.day13)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-instruction [line]
  (let [instruction-string (last (str/split line #" "))
        tokens (str/split instruction-string #"=")]
    {
     :axis (tokens 0)
     :value (u/to-int (tokens 1))
     }
    ))

(defn parse [lines]
  (loop [state :dots
         lines lines
         dots '()
         instructions []]
    (cond
      (empty? lines) {:dots dots :instructions instructions}
      :else (let [line (first lines)]
        (cond
          (empty? line) (recur :instructions (rest lines) dots instructions)
          (= state :dots) (recur :dots (rest lines) (conj dots (mapv u/to-int (str/split line #","))) instructions)
          (= state :instructions) (recur :instructions (rest lines) dots (conj instructions (parse-instruction line))))))))

(defn adjust-for-fold [start fold]
  (- start (* 2 (- start fold))))

(defn fold-dot [dot instruction]
  (let [x (first dot) y (second dot)
        axis (:axis instruction) value (:value instruction)]
    ; (println dot axis value)
    (cond
      (= axis "x") (if (> x value) [(adjust-for-fold x value) y] dot)
      (= axis "y") (if (> y value) [x (adjust-for-fold y value)] dot))))

(defn fold [dots instruction]
  (map #(fold-dot % instruction) dots))

(defn fold-complete [dots instructions]
  (reduce
    (fn [dots instruction] (fold dots instruction))
    dots
    instructions))

(defn dots-to-string [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))
        dots-set (set dots)]
    (for [y (range (inc max-y))]
      (for [x (range (inc max-x))]
        (if (contains? dots-set [x y]) "#" ".")))))

(defn print-dots [dots]
  (println (str/join \newline (map #(str/join %) (dots-to-string dots)))))

(defn solution [file]
  (let [data (u/read-file (str "./data/2021/day13/" file ".txt"))
        parsed (parse data)
        dots (:dots parsed)
        instructions (:instructions parsed)]
    {
     :part1 (let [instruction (first instructions)
                  folded (fold dots instruction)]
              (count (frequencies folded)))
     :part2 (fold-complete dots instructions)
     }))