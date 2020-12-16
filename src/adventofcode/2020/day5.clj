(ns adventofcode.2020.day5)
(require '[adventofcode.util :as u])

(defn convert-to-partitioning-commands
  [s commands]
  (let [num-parts (count commands)]
    (map #(assoc {} :num-parts num-parts :part (commands %)) s)))

(defn convert-boarding-pass-to-partitioning-commands
  "Converts a boarding pass string into a sequence of partitioning commands
  that can be passed into u/find-by-partitioning"
  [boarding-pass row-part-length column-part-length]
  (let [row-part (take row-part-length boarding-pass)
        column-part (take column-part-length (drop row-part-length boarding-pass))]
    {:row-commands (convert-to-partitioning-commands row-part {\F 1 \B 2})
     :column-commands (convert-to-partitioning-commands column-part {\L 1 \R 2})}))

(defn binary-partitions
  "Converts integer into an array from 0...k where k is 2^(n-1)"
  [num]
  (let [max (int (Math/pow 2 num))
        range (range 0 max)]
    (vec range)))

(defn parse-boarding-pass
  [boarding-pass row-part-length column-part-length]
  (let [possible-rows (binary-partitions row-part-length)
        possible-columns (binary-partitions column-part-length)
        partitioning-commands (convert-boarding-pass-to-partitioning-commands boarding-pass row-part-length column-part-length)
        row-partitioning-command (partitioning-commands :row-commands)
        column-partitioning-command (partitioning-commands :column-commands)]
    (let [row (u/find-by-partitioning possible-rows row-partitioning-command)
          column (u/find-by-partitioning possible-columns column-partitioning-command)]
      {:row row
       :column column
       :seatid (+ (* row 8) column)})))

(def data (u/read-file "./data/day/5/input.txt"))

(defn solution []
  (let [seat-ids (map :seatid (map #(parse-boarding-pass % 7 3) data))]
    {:part1 (reduce max seat-ids)
     :part2 (let [sorted (sort seat-ids)
                  pairs (partition 2 1 sorted)]
              (filter #(not (u/consecutive? %)) pairs))}))
