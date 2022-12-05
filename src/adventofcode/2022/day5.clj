(ns adventofcode.2022.day5)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-move [move-line]
  (let [tokens (str/split move-line #" ")]
    ; 0-indexed from / to
    {
     :from  (dec (u/to-int (nth tokens 3)))
     :to    (dec (u/to-int (nth tokens 5)))
     :count (u/to-int (nth tokens 1))
     }))

(defn move-one [state from to]
  ; (println state "from: " from "to: " to "current: " (state from))
  (let [top (first (state from))]
    (if (nil? top) state
                   (-> state
                       (update-in [to] #(cons top %))
                       (update-in [from] rest)))))

(defn move [state {from :from to :to count :count}]
  (reduce
    (fn [state _] (move-one state from to))
    state
    (range 0 count)))

(defn move-part-2 [state {from :from to :to count :count}]
  ; (println state from to count)
  (-> state
      (update-in [to] #(concat (take count (state from)) %))
      (update-in [from] #(drop count %))))

(defn parse-crate-line [crate-line]
  (let [partitions (partition 4 4 " " crate-line)
        crates (map #(nth % 1) partitions)]
    crates))

(defn add-crates [state crates]
  (let [indexed (u/zip-indexed crates)
        filtered (filter #(not (= (second %) \space)) indexed)]
    (loop [state state
           filtered filtered]
      (cond
        (empty? filtered) state
        :else (let [head (first filtered)
                    index (first head)
                    crate (second head)]
                (recur
                  (update-in state [index] #(cons crate %))
                  (rest filtered)))))))

(defn reverse-crates [state]
  (->> state
       (map (fn [[index crates]] [index (reverse crates)]))
       u/pairs-to-map))

(defn parse [file]
  (loop [lines (u/read-file (str "./data/2022/day5/" file ".txt"))
         state {}
         moves []]
    (let [head (first lines)
          tail (rest lines)]
      (cond
        (empty? lines) {:state (reverse-crates state) :moves moves}
        (str/starts-with? head "move") (recur tail state (conj moves (parse-move head)))
        (or (empty? (str/trim head))                        ; empty line
            (str/starts-with? (str/trim head) "1"))         ; list of stack indexes
        (recur tail state moves)                            ; ignore and move on
        :else (recur tail (add-crates state (parse-crate-line head)) moves)))))

(defn part1 [file]
  (let [{moves :moves state :state} (parse file)
        final-state (reduce move state moves)
        size (count final-state)]
    (apply str (map first (for [index (range size)]
                 (final-state index))))))

(defn part2 [file]
  (let [{moves :moves state :state} (parse file)
        final-state (reduce move-part-2 state moves)
        size (count final-state)]
    (apply str (map first (for [index (range size)]
                            (final-state index))))))
