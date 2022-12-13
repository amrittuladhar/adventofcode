(ns adventofcode.2022.day11)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

; Notes: In order to avoid worry levels from getting so big in part 2 that operations on them take too long,
; we take the product of the all the divisors for each monkey (LCM of the divisors would as well),
; and always scale down worry levels using this product. This works because (a) modular arithmetic is compatible
; with all the operations we need to support (b) we don't actually need to know absolute worry levels, only how
; many inspections each monkey performed.

(def OPS {"*" * "+" + "-" - "/" /})

(defn parse-var [var-name old]
  (if (= var-name "old") old (u/to-int var-name)))

(defn parse-operation [operation]
  ; parses "new = old + old" or "new = old * 19"
  (let [[_ _ var1 op var2] (str/split operation #" ")
        op-fn (OPS op)]
    {:op
     (fn [old]
       (op-fn (parse-var var1 old) (parse-var var2 old)))
     :description operation
     }))

(defn parse-target [target]
  ; parses "    If true: throw to monkey 2"
  (let [tokens (str/split target #" ")]
    (u/to-int (last tokens))))

(defn parse-divisor [test]
  ; parses "     Test: divisible by 19"
  (let [tokens (str/split test #"Test: divisible by ")
        div (u/to-int (last tokens))]
    div))

(defn parse-starting-items [starting-items]
  (let [[_ items] (str/split starting-items #"Starting items: ")]
    (map u/to-long (str/split items #", "))))

(defn parse-group [[monkey starting-items operation test if-true if-false empty-line]]
  {
   :monkey    monkey
   :items     (parse-starting-items starting-items)
   :operation (-> operation (str/split #":") second str/trim parse-operation)
   :div       (parse-divisor test)
   :if-true   (parse-target if-true)
   :if-false  (parse-target if-false)
   :count     0
   }
  )

(defn find-updates [bored-factor
                    {items                   :items
                     {op :op d :description} :operation
                     common-mod              :common-mod
                     div                     :div
                     if-true                 :if-true
                     if-false                :if-false}]
  "Given a monkey and the worry factor decrease, finds new worry levels and new monkeys for existing items"
  (map
    (fn [item]
      ; (prn "find-updates" item "operation" d)
      (let [
            new-level (-> (op item)                         ; apply operation
                          (/ bored-factor)                  ; apply bored factor
                          Math/floor                        ; round down
                          long                              ; convert to long
                          (mod common-mod))                 ; scale down by product of all divisors
            ]
        (if (= (rem new-level div) 0)
          [if-true new-level]
          [if-false new-level])))
    items))

(defn update-passer [monkeys passer]
  "Update a passer monkey's list of items and item count"
  ; (prn "update-passer" "passer:" passer "monkeys:" monkeys)
  (update monkeys passer
          (fn [monkey] (-> monkey
                           (update :count #(+ % (count (:items monkey))))
                           (update :items (constantly '()))))))

(defn pass-items [monkeys passer updates]
  (as-> monkeys monkeys
        ; update passing monkey's data
        (update-passer monkeys passer)
        ; update receiving monkeys' data
        (reduce
          (fn [monkeys [new-monkey-index new-item]]
            (update monkeys new-monkey-index (fn [existing-monkey]
                                               ; (prn new-monkey-index "new-item" new-item "div" div "modded" modded)
                                               (update existing-monkey :items #(cons new-item %)))))
          monkeys
          updates)))

(defn play-keep-away [monkeys factor]
  "one round of keep-away"
  (let [size (count monkeys)]
    (loop [index 0 monkeys monkeys]
      (cond
        (= index size) monkeys
        :else (let [monkey (nth monkeys index)
                    updates (find-updates factor monkey)]
                ; (prn "keep-away" "index:" index "monkey:" monkey "updates:" updates)
                (recur
                  (inc index)
                  (pass-items monkeys index updates)))))))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day11/" file ".txt"))]
    (let [monkeys (->> (partition 7 7 "" lines)
                       (map parse-group))
          common-mod (->> (map :div monkeys)
                          (reduce *))]
      (apply vector (map
                      #(update % :common-mod (constantly common-mod))
                      monkeys)))))

(defn monkey-business-level [monkeys rounds bored-factor]
  (->> (reduce
         (fn [m _] (play-keep-away m bored-factor))
         monkeys
         (range 1 (inc rounds)))
       (map :count)
       (sort >)
       (take 2)
       (reduce *)))

(defn part1 [file]
  (let [monkeys (parse file)]
    (monkey-business-level monkeys 20 3)))

(defn part2 [file]
  (let [monkeys (parse file)]
    (monkey-business-level monkeys 10000 1)))
