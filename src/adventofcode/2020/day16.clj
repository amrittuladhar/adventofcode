(ns adventofcode.2020.day16)
(require '[adventofcode.util :as u]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-rule
  [rule-str]
  (let [tokens (str/split rule-str #"-")
        min (u/to-int (first tokens))
        max (u/to-int (second tokens))]
    {:min min :max max}))

(defn parse-rule-line
  [line]
  (let [tokens (str/split line #": ")
        field (first tokens)
        rules (second tokens)
        rules-seq (str/split rules #" or ")
        parsed-rules (map parse-rule rules-seq)]
    ; (printf "field: %s parsed-rules: %s\n" field (vec parsed-rules))
    {:field field :rules parsed-rules}))

(defn parse-ticket
  [line]
  (map u/to-int (str/split line #",")))

(defn apply-rule
  [rule value]
  (and (>= value (:min rule)) (<= value (:max rule))))

(defn apply-rule-to-ticket
  [rule ticket]
  (loop [t ticket result {}]
    (cond
      (empty? t) result
      (apply-rule rule (first t)) (recur (rest t) (update-in result [:valid] #(conj % (first t))))
      :else (recur (rest t) (update-in result [:invalid] #(conj % (first t)))))))

(defn is-valid?
  [rule ticket]
  (empty? (:invalid (apply-rule-to-ticket rule ticket))))

(defn parse
  ([lines rules my-ticket nearby-tickets current-region]
   (let [f (first lines) r (rest lines)]
     ; (printf "f: %s current-region: %s\n" f current-region)
     (cond
       (empty? lines) {:rules rules :my-ticket my-ticket :nearby-tickets nearby-tickets}
       (empty? f) (parse r rules my-ticket nearby-tickets current-region)
       (= f "your ticket:") (parse r rules my-ticket nearby-tickets :my-ticket)
       (= f "nearby tickets:") (parse r rules my-ticket nearby-tickets :nearby-tickets)
       :else (case current-region
               :rules (parse r (conj rules (parse-rule-line f)) my-ticket nearby-tickets current-region)
               :my-ticket (parse r rules (parse-ticket f) nearby-tickets current-region)
               :nearby-tickets (parse r rules my-ticket (conj nearby-tickets (parse-ticket f)) current-region)))))
  ([lines]
   (parse lines '() nil '() :rules)))

(defn validate-tickets
  [rules tickets]
  (map (fn [ticket] (map #(apply-rule-to-ticket % ticket) rules)) tickets))

(defn find-invalid-ticket-parts
  [rules ticket]
  (filter (fn [ticket-part] (every? #(not (apply-rule % ticket-part)) rules)) ticket))

(defn find-valid-tickets
  [rules tickets]
  (for [ticket tickets
        :when (empty? (find-invalid-ticket-parts rules ticket))]
    ticket))

(def data-small-2 (parse (u/read-file "./data/day/16/data-small-2.txt")))
(def data-small (parse (u/read-file "./data/day/16/data-small.txt")))
(def data (parse (u/read-file "./data/day/16/input.txt")))

(defn invalid-fields
  "given field-rules, which fields are the given value invalid for?"
  [field-rules value]
  (for [field-rule field-rules
        :let [field (:field field-rule)
              rules (:rules field-rule)]
        :when (every? #(not (apply-rule % value)) rules)]
    field))

(defn remove-invalid-fields
  [; field1 -> (0, 1), field2 -> (1, 2)
   field-possible-indexes
   ; 0 -> (field1, field3), 1 -> (field2, field3)
   invalid-field-indexes]
  (loop
    [current-possible field-possible-indexes
     current-invalid invalid-field-indexes]
    (let [f (first current-invalid)
          invalid-index (first f)
          fields (second f)
          remaining (rest current-invalid)]
      ;(printf "current-possible: %s (%s) f: %s invalid-index: %s fields: %s\n"
      ;        current-possible (type current-possible) f invalid-index (vec fields))
      (if (empty? current-invalid)
        current-possible
        (recur
          (u/update-vals current-possible
                         fields
                         (fn [existing] (set (remove #(= % invalid-index) existing))))
          remaining)))))

(defn fields-resolved?
  [fields-possible-indexes]
  (let [values (map vals fields-possible-indexes)]
    ; (println "checking " values "\n")
    (every? #(= (count %) 1) values)))

(defn resolve-fields
  [field-rules valid-tickets]
  (let [fields (map :field field-rules)
        initial-possible-indexes (range 0 (count (first valid-tickets)))
        ; initially start with a field possibly being at any column
        ; e.g. field1 -> (0, 1, 2); field2 -> (0, 1, 2)
        fields-possible-indexes (u/into-map fields (set initial-possible-indexes))]
    (loop [remaining-valid-tickets valid-tickets
           current fields-possible-indexes]
      ;(printf "current: %s\n" current)
      (cond
        (empty? remaining-valid-tickets) current
        ; if our map of possible indexes has one value for each field, we're done
        (fields-resolved? current) current
        :else (let [ticket (first remaining-valid-tickets)
                    rest-tickets (rest remaining-valid-tickets)
                    ; map of indexes to list of field names which are invalid at that index
                    ; e.g. 0 -> (field1, field2); 1 -> (field2); 2 -> (field1, field3)
                    invalid-map-pairs (map-indexed (fn [i v] [i (invalid-fields field-rules v)]) ticket)
                    invalid-field-indexes (u/pairs-to-map invalid-map-pairs)]
                ;(printf "ticket %s\n invalid-field-indexes %s\n\n" (vec ticket) invalid-field-indexes)
                (recur rest-tickets (remove-invalid-fields current invalid-field-indexes)))))))

(defn eliminate-duplicates
  [fields-possible-indexes]
  (let [all-fields (set (keys fields-possible-indexes))]
    (loop [current fields-possible-indexes
           iterations 0]
      ; (printf "Current: %s\n" current)
      (let [resolved-fields (u/pairs-to-map (filter (fn [[_ indexes]] (= 1 (count indexes))) current))]
        (cond
          (= (count resolved-fields) (count all-fields)) (u/pairs-to-map (map (fn [[field index]] [field (first index)]) current))
          (>= iterations 500) (throw (IllegalStateException. (format "Could not complete in %s iterations" iterations)))
          :else (let [resolved-field-names (set (keys resolved-fields))
                      resolved-field-indexes (set (map first (vals resolved-fields)))
                      unresolved-field-names (set/difference all-fields resolved-field-names)
                      all-unresolved-map (u/pairs-to-map (for [index resolved-field-indexes] [index unresolved-field-names]))]
                  ;(printf "resolved-fields: %s resolved-field-indexes: %s unresolved: %s unresolved-map: %s\n"
                  ;        resolved-field-names resolved-field-indexes unresolved-field-names all-unresolved-map)
                  (recur (remove-invalid-fields current all-unresolved-map)
                         (inc iterations))))))))

(defn ticket-field
  [field-indexes field ticket]
  (nth ticket (field-indexes field)))

(defn solution
  ([data]
   (let [rules (:rules data)
         all-rules (flatten (map :rules rules))
         my-ticket (:my-ticket data)
         nearby-tickets (:nearby-tickets data)
         nearby-valid-tickets (find-valid-tickets all-rules nearby-tickets)]
     {:part1 (reduce + (flatten (map #(find-invalid-ticket-parts all-rules %) nearby-tickets)))
      :part2 (let [field-indexes (eliminate-duplicates (resolve-fields rules nearby-valid-tickets))
                   departure-fields (u/filter-keys field-indexes #(str/starts-with? % "departure"))
                   departure-field-names (keys departure-fields)]
               (reduce * (map #(ticket-field departure-fields % my-ticket) departure-field-names)))}))
  ([]
   (solution data)))