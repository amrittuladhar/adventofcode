(ns adventofcode.2023.day2)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-record [record-string]
  (let [tokens (str/split (str/trim record-string) #" ")]
    [(keyword (second tokens)) (u/to-int (first tokens))]))

(defn parse-records [records-string]
  (let [tokens (str/split records-string #",")]
    (into {} (map parse-record tokens))))

(defn parse-line [line]
  (let [tokens (str/split line #":")
        game (u/to-int (second (str/split (first tokens) #" ")))
        records (str/split (second tokens) #";")]
    {:game    game
     :records (map parse-records records)}))

(defn matches? [game max-red max-green max-blue]
  (let [game-id (:game game)
        records (:records game)]
    (every?
      (fn [record]
        (and
          (< (u/if-nil? (:blue record) 0) (inc max-blue))
          (< (u/if-nil? (:red record) 0) (inc max-red))
          (< (u/if-nil? (:green record) 0) (inc max-green))))
      records)))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2023/day2/" file ".txt"))]
    (map parse-line lines)))

(defn part1 [file]
  (let [games (parse file)
        matching-games (filter #(matches? % 12 13 14) games)
        matching-game-ids (map :game matching-games)]
    (reduce + matching-game-ids)))

; Part 2 stuff
(defn min-cubes [game]
  (let [records (:records game)]
    (reduce
      (fn [current record]
        (let [max-blue (:blue current)
              max-red (:red current)
              max-green (:green current)]
          {
           :blue  (max max-blue (u/if-nil? (:blue record) 0))
           :red   (max max-red (u/if-nil? (:red record) 0))
           :green (max max-green (u/if-nil? (:green record) 0))
           }
          )
        )
      {:blue 0 :red 0 :green 0}
      records)))

(defn power [cubes]
  (reduce * (map second cubes)))

(defn part2 [file]
  (let [games (parse file)
        min-cubes (map min-cubes games)
        powers (map power min-cubes)]
    (reduce + powers)))
