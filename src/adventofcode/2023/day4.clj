(ns adventofcode.2023.day4
  (:require [clojure.set :as set]))
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-numbers [number-string]
  (->> (str/split number-string #" ")
       (filter #(not (empty? %)))
       (map str/trim)
       (map u/to-int)))

(defn parse-card [line]
  (let [card-numbers (str/split line #":")
        card (u/to-int (last (str/split (first card-numbers) #" ")))
        numbers (second card-numbers)
        winning-have (str/split numbers #"\|")]
    (let [
        winning (parse-numbers (first winning-have))
        have (parse-numbers (second winning-have))]
    {
     :card card
     :winning winning
     :have have
     }
    )
  ))

(defn score [{_ :card winning :winning have :have}]
  (count (set/intersection (set winning) (set have))))

(defn calculate-points [card]
  (let [score (score card)]
    (if (= score 0) 0 (Math/pow 2 (dec score)))))

(defn parse-cards [file]
  (let [lines (u/read-file (str "./data/2023/day4/" file ".txt"))]
    (map parse-card lines)
    ))

(defn part1 [file]
  (let [cards (parse-cards file)
        card-points (map calculate-points cards)]
    (reduce + card-points)))

; Part 2 stuff

(defn cards-by-idx [cards idx]
  (filter #(= (:card %) idx) cards))

(defn first-card-by-idx [cards idx]
  (first (cards-by-idx cards idx)))

(defn process-cards [cards]
  (loop [cards cards
         card-idx 1]
    (let [current-cards (cards-by-idx cards card-idx)]
      ; (println cards)
      ; (println "Processing" current-cards)
      (cond
        (empty? current-cards) cards
        :else (let [score (score (first current-cards))
                    card-indexes-to-add (range (inc card-idx) (inc (+ card-idx score)))
                    cards-to-add (flatten (map #(first-card-by-idx cards %) card-indexes-to-add))
                    all-cards-to-add (flatten (repeat (count current-cards) cards-to-add))
                    ]
                ; (println "idx" card-idx "score" score "count" (count current-cards) "to-add" (map :card cards-to-add))
                (recur (concat cards all-cards-to-add) (inc card-idx)))))))

(defn part2 [file]
  (let [cards (parse-cards file)]
    (count (process-cards cards))))
