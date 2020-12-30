(ns adventofcode.2020.day21
  (:require [clojure.set :as set]))
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-line
  [line]
  (let [tokens (str/split line #"\(contains ")
        allergens-part (second tokens)
        allergens (str/split allergens-part #", |\)")]
    {:ingredients (str/split (first tokens) #" ")
     :allergens   allergens}))

(defn intersect-with
  [existing new-set]
  ; (printf "existing: %s new-set: %s\n" existing new-set)
  (if (nil? existing) new-set (set/intersection existing new-set)))

(defn inc-nil
  [n]
  (if (nil? n)
    1
    (inc n)))

(defn flatten-sets
  [seq]
  (set (flatten (map #(into '() %) seq))))

(defn process-ingredients
  [data-seq]
  (loop [loop-data-seq data-seq
         allergens-map {}
         all-ingredients {}]
    (if (empty? loop-data-seq)
      {:allergens-map   allergens-map
       :all-ingredients all-ingredients}
      (let [head (first loop-data-seq)
            tail (rest loop-data-seq)
            ingredients (set (:ingredients head))
            allergens (:allergens head)]
        (do
          ; (printf "ing: %s allg: %s\n" ingredients allergens)
          (let [new-map (u/update-vals allergens-map allergens intersect-with ingredients)]
                ; (printf "   new: %s\n" new-map)
                (recur tail new-map (u/update-vals all-ingredients ingredients inc-nil))))))))

(defn partition-by-resolved
  [allergens-map]
  (loop [loop-map allergens-map
         resolved {}
         unresolved {}]
    (if (empty? loop-map)
      {:resolved resolved :unresolved unresolved}
      (let [head (first loop-map)]
        (if (= (count (second head)) 1)
          (recur (rest loop-map) (conj resolved head) unresolved)
          (recur (rest loop-map) resolved (conj unresolved head)))))))

(defn resolve-allergens
  [allergens-map]
  (loop [loop-map allergens-map]
    (let [partitioned (partition-by-resolved loop-map)
          resolved (:resolved partitioned)
          unresolved (:unresolved partitioned)]
      (if (empty? unresolved)
        ; resolved map has sets as values so unwrap the sets
        ; e.g. "allergen1" -> #{ "ingredient1" }  TO  "allergen1" -> "ingredient1"
        (map (fn [[key valset]] (vector key (first valset))) loop-map)
        ; remove all resolved ingredients from list of possible ingredients for other allergens
        (let [resolved-ingredients (flatten-sets (vals resolved))
              unresolved-allergens (keys unresolved)
              updated-map (u/update-vals loop-map unresolved-allergens #(set/difference % resolved-ingredients))]
          ;(printf "resolved: %s\n resolved-ingredients: %s\n unresolved: %s\n unresolved allergens:%s\n new-map:%s\n\n"
          ;        resolved resolved-ingredients unresolved unresolved-allergens updated-map)
          (recur updated-map))))))

(def data-small (map parse-line (u/read-file "./data/day/21/input-small.txt")))
(def data (map parse-line (u/read-file "./data/day/21/input.txt")))

(defn solve
  ([data]
   (let [processed (process-ingredients data)
         allergens-map (:allergens-map processed)]
     {:part1 (let [allergic-ingredients (flatten-sets (vals allergens-map))
                   all-ingredients (:all-ingredients processed)
                   non-allergic-ingredients (filter (fn [[key _]] (not (contains? allergic-ingredients key))) all-ingredients)]
               (reduce (fn [sum [_ count]] (+ sum count)) 0 non-allergic-ingredients))

      :part2 (let [resolved-allergens (resolve-allergens allergens-map)
                   sorted-by-allergen (sort-by first resolved-allergens)
                   dangerous-ingredients (map second sorted-by-allergen)]
               (str/join "," dangerous-ingredients))}))
  ([]
   (solve data)))