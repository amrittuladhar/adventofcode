(ns adventofcode.2020.day22)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn pick-first-card
  [[player deck]]
  [player (first deck)])

(defn drop-first-card
  [[player deck]]
  [player (vec (rest deck))])

(defn pick-cards-for-recursive-combat
  [[player deck]]
  (let [n (first deck)]
    [player (vec (take n (rest deck)))]))

(defn give-winner-cards
  [decks winner cards]
  (let [; remove the first card from all decks
        top-removed (u/update-vals decks (keys decks) #(subvec % 1))
        ; add all cards to winner
        winner-added (update-in top-removed [winner] (fn [current-vec] (vec (concat current-vec cards))))]
    ; (printf "new deck: %s\n" winner-added)
    winner-added))

(defn regular-combat
  "given map of decks, picks the winner based on whoever has the highest top card and redistributes cards"
  [decks _ _]
  (let [first-cards (u/pairs-to-map (map pick-first-card decks))
        sorted (u/sort-map-by-values first-cards <)
        winner (first (first sorted))
        all-cards (vals sorted)]
    ; (printf "[regular] decks: %s winner: %s first-cards: %s sorted: %s\n" decks winner first-cards sorted)
    (give-winner-cards decks winner all-cards)))

(defn enough-cards-remaining?
  [[_ deck]]
  (if (empty? deck)
    false
    (>= (count (rest deck)) (first deck))))

(defn play
  "game-fn is a function that takes a map of decks and returns a new map after playing a round"
  ([game-fn decks max-iterations]
   (play game-fn decks max-iterations #{}))
  ([game-fn decks max-iterations old-decks]
   (loop [current-decks decks
          iterations max-iterations
          current-old-decks old-decks]
     (let [non-empty-decks (filter (fn [[_ cards]] (> (count cards) 0)) current-decks)]
       ; (printf "[play]\n\tdecks: %s\n\told: %s\n" current-decks current-old-decks)
       (cond
         (contains? current-old-decks current-decks) {:player "Player 1" :deck (current-decks "Player 1")}
         (= iterations 0) (throw (IllegalStateException. (format "could not complete in %s iterations" max-iterations)))
         (= (count non-empty-decks) 1) (let [winning-deck (first non-empty-decks)]
                                         {:player (first winning-deck) :deck (second winning-deck)})
         :else (recur (game-fn current-decks max-iterations #{}) (dec iterations) (conj current-old-decks current-decks)))))))

; Took a long time because turns out I was copying too many cards for Recursive Combat
; Discovered the issue thanks to https://old.reddit.com/r/adventofcode/comments/khzm6z/2020_day_22_part_2_properties_of_misinterpreted/

(defn recursive-combat
  [decks max-iterations old-decks]
  (loop [current-decks decks]
    ; (printf "[recursive] decks: %s\n" current-decks)
    (cond
      (contains? old-decks current-decks) (assoc {} "Player 1" (current-decks "Player 1"))
      (every? enough-cards-remaining? current-decks)
      (let [result (play recursive-combat (u/pairs-to-map (map pick-cards-for-recursive-combat current-decks)) max-iterations old-decks)
            winner (:player result)
            ; deck sorted so that the winner appears first
            sorted (u/sort-map-by-key-ordering current-decks (list winner))
            all-cards (vec (filter #(not (nil? %)) (map first (vals sorted))))]
        ; (printf "  [recursive] winner: %s all-cards: %s current-decks: %s sorted: %s\n" winner all-cards current-decks sorted)
        (recur (give-winner-cards current-decks winner all-cards)))
      :else (regular-combat current-decks max-iterations old-decks))))

(defn parse
  [lines]
  (loop [loop-lines lines
         decks {}
         current-player nil]
    (let [head (first loop-lines)
          tail (rest loop-lines)]
      (cond
        (empty? loop-lines) decks
        (empty? head) (recur tail decks nil)
        (re-matches #"^Player \d.*" head) (recur tail decks (re-find #"Player \d" head))
        :else (recur tail (update-in decks [current-player] (fnil conj []) (u/to-int head)) current-player)))))

(def data-small (parse (u/read-file "./data/day/22/input-small.txt")))
(def data (parse (u/read-file "./data/day/22/input.txt")))
(def data-recursive (parse (str/split "Player 1:\n43\n19\n\nPlayer 2:\n2\n29\n14" #"\n")))

(defn calculate-score
  [deck]
  (let [deck-size (count deck)
        scores (map-indexed (fn [index card] (* (- deck-size index) card)) deck)]
    (reduce + scores)))

(defn solve
  ([data]
   {:part1 (let [winner (play regular-combat data 1000)] (calculate-score (:deck winner)))
    :part2 (let [winner (play recursive-combat data 100000)] (calculate-score (:deck winner)))})
  ([]
   (solve data)))