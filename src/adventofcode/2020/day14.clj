(ns adventofcode.2020.day14)
(require
  '[clojure.string :as str]
  '[adventofcode.util :as u])

(defn parse-op
  [line]
  (let [tokens (str/split line #" = ")
        value (u/to-int (second tokens))
        matches (re-matches #"mem\[(\d+)\]" (first tokens))]
    {:value value :address (u/to-int (second matches))}))

(defn parse-mask
  [line]
  (let [matches (re-matches #"^mask = (.+)$" line)]
    (second matches)))

(defn apply-mask-to-bit
  [[mask bit]]
  (cond
    (= mask \X) bit
    :else mask))

(defn apply-mask-to-bit-2
  [[mask bit]]
  (cond
    (= mask \X) \X
    (= mask \1) \1
    (= bit \1) \1
    :else 0))

(defn apply-mask-part-1
  [mask operation]
  (let [mask-length (count mask)
        address (:address operation)
        value (:value operation)
        binary (u/pad-left (Long/toBinaryString value) mask-length "0")]
    {address (Long/parseLong (str/join (map apply-mask-to-bit (u/zip mask binary))) 2)}))

; Say the masked address is 1X01X0X
; There are 3 Xs so we consider binary representations
; of all numbers from 0 to 2^3 which gives us all
; 3-length permutations of 0 and 1, i.e 000, 001, 010, ... 111
; Then we substitute each digit from these permutations into
; the Xs in the address to yield
; 1001000
; 1001001
; 1001100
; and so on
(defn replace-masked-address
  [address bits]
  (loop [addr address
         bs bits
         acc ""]
    (let [f (first addr) r (rest addr)]
      (cond
        (empty? addr) acc
        (= f \X) (recur r (rest bs) (str acc (first bs)))
        :else (recur r bs (str acc f))))))

(defn expand-masked-address
  [address]
  (if (empty? address)
    nil
    (let [count-x (count (filter #(= % \X) address))
          permuts (map #(Integer/toBinaryString %) (range 0 (Math/pow 2 count-x)))
          padded-permuts (map #(u/pad-left % count-x "0") permuts)]
      ; (printf "count-x: %s permuts: %s padded-permuts: %s\n" count-x (vec permuts) (vec padded-permuts))
      (map #(replace-masked-address address %) padded-permuts))))

(defn apply-mask-part-2
  [mask operation]
  (let [mask-length (count mask)
        address (:address operation) value (:value operation)
        address-binary (u/pad-left (Long/toBinaryString address) mask-length "0")
        masked-address (map apply-mask-to-bit-2 (u/zip mask address-binary))
        expanded-addresses (expand-masked-address masked-address)
        expanded-addresses-longs (map #(Long/parseLong % 2) expanded-addresses)]
    ;(printf "address: %s\n mask: %s\n address-binary: %s\n masked-address: %s\n expanded-addresses: %s\n longs: %s\n\n"
    ;        address mask address-binary (str (vec masked-address)) (vec expanded-addresses) (vec expanded-addresses-longs))
    (into {} (map #(vector % value) expanded-addresses-longs))))


(def data (u/read-file "./data/day/14/input.txt"))
(def data-small (u/read-file "./data/day/14/input_small.txt"))

(defn process
  ([all-data apply-mask-fn]
   (loop [data all-data
          storage {}
          mask nil]
     (let [first-data (first data)
           rest-data (rest data)]
       (cond
         (empty? data) storage
         (nil? mask) (recur rest-data storage (parse-mask first-data))
         :else (let [new-mask (parse-mask first-data)]
                 (if (nil? new-mask)
                   (let [op (parse-op first-data)]
                     (recur rest-data (merge storage (apply-mask-fn mask op)) mask))
                   (recur rest-data storage new-mask))))))))

(defn solution
  ([input]
   {:part1 (reduce + (vals (process input apply-mask-part-1)))
    :part2 (reduce + (vals (process input apply-mask-part-2)))})
  ([]
   (solution data)))

; part 2 - 1576953804222 - too low