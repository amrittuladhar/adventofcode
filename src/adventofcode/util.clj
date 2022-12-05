(ns adventofcode.util)
(require '[clojure.string :as str]
         '[clojure.math.combinatorics :as combo])
(use 'clj-stacktrace.core)

;; ====================== utils ==================
(def ^:dynamic *verbose* false)

(defmacro printfv
  "Simple debugging macro to print out values"
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro letp
  [bindings & body]
  `(let ~@bindings
     (printf "%s" ~(destructure bindings))
     ~@body))

(defn indent
  [n]
  (str/join (repeat n "\t")))

(defn line-seq-str
  [str]
  (->> str
       java.io.StringReader.
       java.io.BufferedReader.
       line-seq))

; Processing input - BEGIN

(defn file-path [name year day]
  (str "./data/" year "/day" day "/" name ".txt"))

(defn read-file
  "Returns lines from a file as a seq"
  [filename]
  (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename)))))

; Processing input - END

; Sequences - BEGIN

; https://groups.google.com/g/clojure-dev/c/NaAuBz6SpkY
(defn take-upto
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-upto pred (rest s)))))))

(defn partition-seq
  "Creates k-sized tuples from a sequence"
  [seq k]
  (if (< (count seq) k)
    nil
    (lazy-seq (cons (take k seq) (partition-seq (rest seq) k)))))

(defn all-partitions-seq-recur
  [seq k answer]
  (cond
    (empty? seq) answer
    :else (let [first (first seq)
                rest (rest seq)
                next-k (dec k)
                new-answers (map #(cons first %) (partition-seq rest next-k))]
            (lazy-seq (all-partitions-seq-recur rest k (concat answer new-answers))))))

(defn zip
  [& xs]
  (apply map vector xs))

(defn all-partitions-seq
  [seq k]
  (all-partitions-seq-recur seq k '()))

(defn zip-and-reduce
  "zips given seqs and reduces them with the given fn"
  [fn & xs]
  (map #(reduce fn %) (apply zip xs)))

; zips two collections, but if one of them runs out, uses the last element for the rest
(defn zip-with-pad [coll1 coll2]
  (loop [coll1 coll1 last-coll1 nil
         coll2 coll2 last-coll2 nil
         acc '()]
    (cond
      (and (empty? coll1) (empty? coll2)) acc
      (empty? coll1) (recur coll1 last-coll1
                            (rest coll2) (first coll2)
                            (conj acc (list last-coll1 (first coll2))))
      (empty? coll2) (recur (rest coll1) (first coll1)
                            coll2 last-coll2
                            (conj acc (list (first coll1) last-coll2)))
      :else (recur (rest coll1) (first coll1)
                   (rest coll2) (first coll2)
                   (conj acc (list (first coll1) (first coll2)))))))

(defn zip-indexed [coll]
  (map-indexed (fn [idx itm] [idx itm]) coll))

; Sequences - END

(defn count-letter
  "Number of occurrences of a letter in a word"
  [word letter]
  (count (filter #(= letter %) word)))

(defn check-character
  "Checks if the nth character in the word is ch. False if word is shorter than n"
  [word n ch]
  (let [index (dec n)]
    (cond
      (< (count word) n) false
      :else (= (nth word index) ch))))

(defn safe-predicate
  "Wraps a predicate to return false if the input is nil, or even if the predicate throws an exception"
  [predicate]
  #(if (nil? %)
     false
     (try
       (predicate %)
       (catch Exception _ false))))

(defn add-pair-to-map
  "Adds a pair to a given map"
  [map pair]
  (assoc map (first pair) (second pair)))

(defn pairs-to-map
  "Converts sequence of pairs into a map"
  [pairs]
  (reduce add-pair-to-map {} pairs))

(defn reverse-map
  [amap]
  (pairs-to-map (map (fn [[key value]] [value key]) amap)))

(defn update-vals [map keys f & args]
  "updates the map for given keys after modifying the values with f.
  Like update-in but operating on multiple keys."
  (loop [loop-keys keys
         loop-map map]
    ; (printf "loop-keys: %s loop-map: %s\n" loop-keys loop-map)
    (if (empty? loop-keys)
      loop-map
      (recur
        (rest loop-keys)
        (update-in loop-map [(first loop-keys)] #(apply f % args))))))

(defn if-nil? [value default] (if (nil? value) default value))

(defn filter-keys
  [map pred]
  (pairs-to-map (filter (fn [[key _]] (pred key)) map)))

(defn to-int
  "Converts given item into an integer, first turning it into string"
  ([s] (Integer/parseInt (str s)))
  ([radix s] (Integer/parseInt (str s) radix)))

(defn to-long
  "Converts given item into a long, first turning it into string"
  [s] (Long/parseLong (str s)))

(defn next-multiple
  "The smallest integer >= num that's a multiple of factor"
  [num factor]
  (if (= (rem num factor) 0)
    num
    (* factor (inc (quot num factor)))))

(defn regex-match?
  "Does the given string match the given regular expression?"
  [re s]
  (not (nil? (re-matches re s))))

(defn range-match?
  "Is the given number within the given bounds?"
  [s min max]
  (let [s-int (to-int s)]
    (and (>= s-int min) (<= s-int max))))

(defn into-map
  "Creates the map using items from the given sequence as keys, and the given value as the value"
  [keys value]
  (zipmap keys (repeat (count keys) value)))

(defn find-by-partitioning
  "Finds an item in sequence given a list of partitioning 'commands'
  Commands are in the form of maps with two keys:
    :num-parts indicates how many parts to divide the sequence into
    :part indicates which part (1-indexed) to pick to find the item
  E.g. ({:num-parts 2 :part 1} {:num-parts 2 :part 1}) means keep dividing
  the sequence into 2 parts and keep picking the first part"
  [search-space commands]
  (if (empty? commands)
    (first search-space)                                    ; there should only be one left
    (let [first-command (first commands)
          num-parts (:num-parts first-command) part (:part first-command)
          part-index (dec part)
          steps (int (/ (count search-space) num-parts))
          partitions (partition steps search-space)]
      (find-by-partitioning (nth partitions part-index) (rest commands)))))

(defn consecutive? [pair] (= 1 (- (second pair) (first pair))))

(defn seq-pairs
  "
  '(1, 2, 3) -> '( (1, (2, 3)), (2, (3)) )
  '(1, 2, 3, 4) -> '( (1, (2, 3, 4)), (2, (3, 4)), (3, (4)) )
  "
  ([seq min-length acc]
   (if (< (count seq) min-length)
     acc
     (seq-pairs (rest seq) min-length (conj acc (list (first seq) (rest seq))))))
  ([seq min-length]
   (seq-pairs seq min-length nil)))

(defn combinations
  "all k-sized combinations of seq"
  [seq k]
  (let [size (count seq)]
    (cond
      (= size 0) nil
      (< size k) nil
      (= k 0) nil
      (= k 1) (map #(list %) seq)
      (= size k) (list seq)
      :else (let [seq-pairs (reverse (seq-pairs seq k))]
              (mapcat
                #(let [first-item (first %) sub-seq (second %)]
                   (map
                     (fn [sub-combination] (cons first-item sub-combination))
                     (combinations sub-seq (dec k))))
                seq-pairs)))))

;(defn permutations
;  [seq]
;  (let [size (count seq)]
;    (cond
;      (= size 0) nil
;      (= size 1) (list seq)
;      (= size 2) (list seq (reverse seq))
;      :else
;      (loop [current-outer seq
;             result nil
;             count 0]
;        (recur
;          (concat (rest current-outer) [(first current-outer)])
;          (conj result
;            (loop [current current-outer]
;              (let [head (first current) tail (rest current)]
;                (map
;                  (fn [sub-permutation] (cons head sub-permutation))
;                  (permutations tail))))
;          current-outer)
;        )))))

(defn find-summing-numbers
  "Finds the smallest k numbers in the given sequence that add up to n"
  [seq n k]
  (let [sorted (sort seq)
        smaller-than-n (take-while #(<= % n) sorted)]
    (filter #(= (reduce + %) n) (combinations smaller-than-n k))))

(defn cartesian-to-angular
  [[x y]]
  (let [r (Math/sqrt (+ (* x x) (* y y)))
        theta (cond
                (= y 0) 0
                (and (< y 0) (= x 0)) (- (/ Math/PI 2))
                (and (> y 0) (= x 0)) (/ Math/PI 2)
                :else (let [atan (Math/atan (/ y x))]
                        (cond
                          (and (< x 0) (>= y 0)) (+ atan Math/PI) ; 2nd quadrant
                          (and (< x 0) (< y 0)) (- atan Math/PI) ; 3rd quadrant
                          :else atan)))]
    {:r r :theta theta}))

(defn angular-to-cartesian
  ([{r :r theta :theta}]
   (map #(Math/round %) (list (* r (Math/cos theta)) (* r (Math/sin theta))))))

(defn convert-to-radians
  [degrees]
  (Math/toRadians degrees))

; rounded-up and rounded-down quotients for the given division
(defn quotient-pair [num divisor]
  (let [q (quot num divisor)
        r (rem num divisor)]
    [q
     (if (zero? r) q (inc q))
     ]))

(defn relative-to-new-origin
  "Performs fn on some coords as if they were relative to an origin other than (0, 0)"
  [coords new-origin fn]
  (let [relative-to-real-origin (zip-and-reduce - coords new-origin)
        result (fn relative-to-real-origin)]
    ;(printf "coords: %s new-origin: %s relative: %s result: %s\n" (vec coords) (vec new-origin) (vec relative-to-real-origin) (vec result))
    (zip-and-reduce + result new-origin)))

(defn rotate-around-origin
  [degrees coords]
  (let [rotate-radians (convert-to-radians degrees)
        angular-coords (cartesian-to-angular coords)
        r (angular-coords :r) theta (angular-coords :theta)
        new-radians (+ theta rotate-radians)
        new-angular-coords {:r r :theta new-radians}
        new-cartesian-coords (angular-to-cartesian new-angular-coords)]
    ;(printf "degrees: %s\n coords: %s\n rotate-radians: %s\n angular-coords: %s\n r: %s theta: %s\n new-radians: %s\n new-angular-coords: %s\n new-cartesian-coords: %s\n\n"
    ;        degrees (vec coords) rotate-radians angular-coords r theta new-radians new-angular-coords (vec new-cartesian-coords))
    ; (map int new-cartesian-coords)))
    new-cartesian-coords))

(defn rotate-around
  [degrees [x y] [origin-x origin-y]]
  (let [radians (convert-to-radians degrees)
        sine (Math/sin radians) cosine (Math/cos radians)
        x-diff (- x origin-x) y-diff (- y origin-y)]
    (list
      (+ origin-x (- (* cosine x-diff) (* sine y-diff)))
      (+ origin-y (+ (* sine x-diff) (* cosine y-diff))))))

(defn modular-inverse
  [number modulo]
  (loop [n 1]
    (let [product (*' n number)]
      (cond
        (= n modulo) nil
        (= 1 (mod product modulo)) n
        :else (recur (inc n))))))

(defn pad-left
  [s desired-length pad-char]
  (let [length (count s)]
    (cond
      (>= length desired-length) s
      :else (let [diff (- desired-length length)
                  pad (repeat diff pad-char)]
              (str/join (concat pad s))))))

(defn sort-map-by-values
  [map value-comparator-fn]
  (into (sorted-map-by (fn [key1 key2] (value-comparator-fn (map key2) (map key1)))) map))

(defn sort-map-by-key-ordering
  "Sorts map to arrange keys so they are in the order of the given sequence"
  [map key-ordering]
  ; first add any missing keys into key-ordering
  (letfn [(add-rest [map key-ordering]
            (loop [current-map map
                   current-key-ordering key-ordering]
              (if (empty? current-key-ordering)
                (concat key-ordering (keys current-map))
                (recur (dissoc current-map (first current-key-ordering)) (rest current-key-ordering)))))]
    (let [ordering (add-rest map key-ordering)]
      (into (sorted-map-by (fn [key1 key2] (- (.indexOf ordering key1) (.indexOf ordering key2)))) map))))

(defn logb
  [base n]
  (/ (Math/log n) (Math/log base)))

; Matrix operations - BEGIN

(defn parse-into-matrix
  ([lines fn]
   (vec (map #(vec (map fn %)) lines)))
  ([lines]
   (parse-into-matrix lines identity)))

; creates a matrix as a map where keys are co-ordinates (vectors [x y]) and values are values
; the map also contains a :size key where the value is a vector with [col-count row-count]
(defn parse-into-matrix-map
  ([lines]
   (let [row-count (count lines) col-count (count (first lines))]
     (pairs-to-map
       (for [x (range col-count) y (range row-count)]
         [[x y] (nth (nth lines y) x)])))))

(defn matrix-map-dimensions [matrix-map]
  [(apply max (map first (map first matrix-map)))
   (apply max (map second (map first matrix-map)))])

(defn find-in-matrix
  "Finds an item from a matrix (vector of vectors) given its co-ordinates"
  ([mat x y]
   (find-in-matrix mat {:x x :y y}))
  ([mat coords]
   (let [x (:x coords) y (:y coords)]
     (nth (nth mat y) x))))

(defn count-in-matrix
  [matrix filter-fn]
  (count (filter filter-fn (flatten matrix))))

(defn convert-matrix-map-to-vec
  [matrix default-value]
  (let [keys (keys matrix)
        xx (map first keys) yy (map second keys)
        max-x (apply max xx) max-y (apply max yy)
        initial-vector (vec (repeat (inc max-y) (mapv (constantly default-value) (range (inc max-x)))))]
    (reduce
      (fn [v [coords value]]
        ; (println "v:" v "coords:" coords "value:" value)
        (update-in v (reverse coords) (constantly value)))
      initial-vector
      matrix)))

(defn convert-matrix-to-map
  [matrix]
  (pairs-to-map
    (for [y (range (dec (count matrix)))]
      (for [x (range (dec (count (first matrix))))]
        [[x y] (find-in-matrix matrix x y)]))))

(defn print-matrix
  [matrix]
  (println (str/join \newline (map #(str/join %) matrix)))
  matrix)

(defn print-matrix-map
  [matrix-map]
  (print-matrix (convert-matrix-map-to-vec matrix-map nil)))

(defn update-in-matrix-fn
  [mat new-value-fn all-coords]
  (printfv "updating %s\n" all-coords)
  (cond
    (empty? all-coords) mat
    :else (let [coords (first all-coords)
                x (if-nil? (:x coords) (first coords))
                y (if-nil? (:y coords) (second coords))
                cur-value (find-in-matrix mat x y)]
            (recur
              (assoc mat y (assoc (nth mat y) x (new-value-fn cur-value)))
              new-value-fn
              (rest all-coords)))))

(defn update-in-matrix
  [mat coords new-value]
  (printfv "updating %s to %s\n" coords new-value)
  (update-in-matrix-fn mat (fn [_] new-value) [coords]))

(defn matrix-size
  [matrix]
  {:y (count matrix) :x (count (first matrix))})

(defn search-matrix
  [mat search-fn]
  (let [y-size (count mat)
        x-size (count (first mat))
        all-coords (for [x (range x-size) y (range y-size)] [x y])]
    (filter #(search-fn (find-in-matrix mat (first %) (second %))) all-coords)))

(defn valid-coords?
  [max-x max-y x y]
  (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y)))

(defn adjacent-in-matrix-no-diagonals
  "Returns a seq of all valid co-ordinates adjacent to the given co-ordinates"
  ([max-x max-y x y]
   (let [coords (list x y)
         directions '((-1 0) (0 -1) (0 1) (1 0))]
     (filter
       #(valid-coords? max-x max-y (first %) (second %))
       (map #(zip-and-reduce + coords %) directions))))
  ([matrix-size coords]
   (adjacent-in-matrix-no-diagonals (dec (:x matrix-size)) (dec (:y matrix-size)) (:x coords) (:y coords))))

(defn adjacent-in-matrix
  "Returns a seq of all valid co-ordinates adjacent to the given co-ordinates"
  ([max-x max-y x y]
   (let [coords (list x y)
         directions '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 0) (1 -1) (1 1))]
     (filter
       #(valid-coords? max-x max-y (first %) (second %))
       (map #(zip-and-reduce + coords %) directions))))
  ([matrix-size coords]
   (adjacent-in-matrix (dec (:x matrix-size)) (dec (:y matrix-size)) (:x coords) (:y coords))))

;; n-dimensions
(defn find-neighbors
  [coords]
  (let [num-dimensions (count coords)
        ; build sequence of numbers that the coordinates have to be adjusted by for getting neighbors
        ; e.g., for 2 dimensions: (-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 0) (1 -1)
        adjustments-base (flatten (repeat num-dimensions '(-1 0 1)))
        all-zeros (repeat num-dimensions 0)
        adjustments (filter #(not= % all-zeros) (combo/permuted-combinations adjustments-base num-dimensions))]
    (mapv #(apply vector (zip-and-reduce + coords %)) adjustments)))

; MATRIX Operations - END
