(ns adventofcode.2021.day4)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn apply-number-to-row [row number]
  (mapv #(if (= (:number %) number) {:number number :marked true} %) row))

(defn apply-number-to-board [board number]
  (mapv #(apply-number-to-row % number) board))

; assume square board
(defn transpose [board]
  (loop [index 0 columns []]
    (if
      (= index (count (first board)))
      columns
      (recur (inc index) (conj columns (mapv #(nth % index) board))))))

(defn bingo? [board]
  (or
    ; check rows
    (> (count (filter #(every? :marked %) board)) 0)
    ; check columns
    (> (count (filter #(every? :marked %) (transpose board))) 0)))

(defn map-to-board-item [item]
  {:number (u/to-int item) :marked false})

; return first board that has bingo
(defn play-bingo [numbers boards]
  (loop [numbers numbers
         called-number nil
         boards boards]
    (let [bingoed-boards (filter bingo? boards)]
      (cond
        (not (empty? bingoed-boards)) {:board (first bingoed-boards) :called-number called-number}
        (empty? numbers) {:board nil :called-number nil}
        :else (recur
                (rest numbers)
                (first numbers)
                (map #(apply-number-to-board % (first numbers)) boards))))))

; returns all boards that have bingo. Each entry in the result will be a map with :board and :called-number
(defn play-bingo-part-2 [numbers boards]
  (loop [numbers numbers
         last-called-number nil
         bingos '()
         boards boards]
    (let [bingoed-boards (filter bingo? boards)
          non-bingoed-boards (filter #(not (bingo? %)) boards)]
      (cond
        (empty? numbers) bingos
        :else (recur
                (rest numbers)
                (first numbers)
                (concat bingos (map #(into {} [[:board %] [:called-number last-called-number]]) bingoed-boards))
                (map #(apply-number-to-board % (first numbers)) non-bingoed-boards))))))

(defn board-value [board called-number]
  (let [unmarked-items (filter #(false? (:marked %)) (flatten board))
        unmarked-numbers (map :number unmarked-items)]
    (* (reduce + unmarked-numbers) called-number)))

(defn read-data [lines]
  ; states: :start :empty :board
  (loop
    [lines lines
     state :start
     numbers []
     boards []
     current-board nil]
    (cond
      (empty? lines) {:numbers numbers :boards (conj boards current-board)}
      :else (let [line (str/trim (first lines))]
              (if (empty? line)
                (case state
                  :start (recur (rest lines) :start numbers boards current-board)
                  :empty (recur (rest lines) :empty numbers boards current-board)
                  :board (recur (rest lines) :empty numbers (conj boards current-board) []))
                (case state
                  :start (recur (rest lines) :empty (mapv u/to-int (str/split line #",")) boards current-board)
                  (recur (rest lines) :board numbers boards (conj current-board (mapv map-to-board-item (str/split line #"\s+"))))))))))

(defn solution []
  (let [data (u/read-file "./data/2021/day4/input.txt")
        parsed (read-data data)
        boards (:boards parsed)
        numbers (:numbers parsed)

        result (play-bingo numbers boards)
        results (play-bingo-part-2 numbers boards)
        last-board (last results)]
    (println results)
    {
     :part1 (board-value (:board result) (:called-number result))
     :part2 (board-value (:board last-board) (:called-number last-board))
     }))