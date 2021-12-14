(ns adventofcode.2021.day14)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-rules [lines]
  (loop [lines lines
         rules {}]
    (cond
      (empty? lines) rules
      (empty? (first lines)) (recur (rest lines) rules)
      :else (recur
              (rest lines)
              (let [tokens (str/split (first lines) #" -> ")]
                (update-in rules [(tokens 0)] (fn [_] (tokens 1))))))))

(defn parse-file [file]
  (let [data (u/read-file (str "./data/2021/day14/" file ".txt"))
        template (first data)
        rules (parse-rules (rest data))]
    {
     :template template
     :rules    rules
     }))

(def apply-rule
  (memoize (fn [[pair rule]]
             (cond
               (nil? rule) (str/join pair)
               :else (str (first pair) rule (second pair))))))

(defn combine-polymer [parts]
  (reduce
    (fn [polymer next] (str polymer (str/join (rest next))))
    (first parts)
    (rest parts)))

(defn apply-rules-part-1 [template rules]
  (let [pairs (map str/join (partition 2 1 template))
        applied-rules (map rules pairs)
        pairs-with-rules (u/zip pairs applied-rules)]
    (combine-polymer (map apply-rule pairs-with-rules))))

(defn run-rules [steps template rules apply-fn]
  (reduce
    (fn [template step]
      (do
        ; (println step template \newline)
        (apply-fn template rules)))
    template
    (range steps)))

(defn part1 [file steps]
  (let [data (parse-file file)
        polymer (run-rules steps (:template data) (:rules data) apply-rules-part-1)
        freqs (frequencies polymer)
        max-freq (reduce max (map second freqs))
        min-freq (reduce min (map second freqs))]
    ; (println max-freq min-freq)
    (- max-freq min-freq)))

; Part 2 - Store the template as a map of pair frequencies instead of as a string

; returns updates to apply to the map as a result of the given rule
(defn get-rules-to-apply [template-map [pair insert]]
  (let [existing-count (template-map pair)
        new-pair-1 (str (first pair) insert)
        new-pair-2 (str insert (second pair))]
    (cond
      (nil? existing-count) '()
      :else (list [pair (- existing-count)] [new-pair-1 existing-count] [new-pair-2 existing-count]))))

(defn apply-rules-part-2 [template-map rules]
  (do
    ; (println "current: " template-map)
    ; calculate all updates based on the initial value of the map
    (let [updates (apply concat (map #(get-rules-to-apply template-map %) rules))]
      ; (println "all updates: " updates)
      ; reduce with the updates, and remove pairs that have been removed
      (u/pairs-to-map
        (filter
          (fn [[_ c]] (> c 0))
          (reduce
            (fn [m [pair adjust]]
              (update m pair (fnil #(+ % adjust) 0)))
            template-map
            updates))))))

(defn count-letters [template-map]
  (reduce
    (fn [map [pair count]] (update map (first pair) (fnil (partial + count) 0)))
    {}
    template-map))

(defn create-template-map [template]
  (u/update-vals
    {}
    (map str/join (partition 2 1 template))
    (fnil inc 0)))

(defn part2 [file steps]
  (let [data (parse-file file)
        template (:template data)
        rules (:rules data)                                 ;
        template-map (create-template-map template)]
    (let [counts (-> (run-rules steps template-map rules apply-rules-part-2)
                     count-letters
                     (update (last template) inc)           ; add 1 for the last letter in the template
                     (u/sort-map-by-values >))]
      (- (second (last counts)) (second (first counts))))))

