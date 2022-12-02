(ns adventofcode.2022.day2)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def WINNING-SHAPES {:R :P :P :S :S :R})

(def SHAPE-SCORES {:R 1 :P 2 :S 3})
(def OPPONENT {"A" :R "B" :P "C" :S})
(def ME {"X" :R "Y" :P "Z" :S})

(defn win? [opponent me]
  (= me (WINNING-SHAPES opponent)))

(defn score-game [opponent me]
  (let [shape-score (SHAPE-SCORES me)]
    (cond
      (= opponent me) (+ 3 shape-score)
      :else (if (win? opponent me) (+ 6 shape-score) (+ 0 shape-score)))))

(defn parse-line [line]
  (let [tokens (str/split line #" ")]
    [(OPPONENT (first tokens)) (ME (second tokens))]))

(defn parse-line-2 [line]
  (let [tokens (str/split line #" ")
        opponent (OPPONENT (first tokens))
        me-move (second tokens)]
    [opponent
     (case me-move
      "X" (WINNING-SHAPES (WINNING-SHAPES opponent)) ; need to lose
      "Y" opponent ; need to draw
      "Z" (WINNING-SHAPES opponent) ; need to win
      )]))

(defn parse [file parse-fn]
  (let [lines (u/read-file (str "./data/2022/day2/" file ".txt")) games []]
    (map parse-fn lines)))

(defn score [file parse-fn]
  (let [data (parse file parse-fn)]
    (reduce + (map #(score-game (first %) (second %)) data))))

(defn part1 [file]
  (score file parse-line))

(defn part2 [file]
  (score file parse-line-2))
