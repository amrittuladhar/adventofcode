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

(defn read-file
  "Returns lines from a file as a seq"
  [filename]
  (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename)))))

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

(defn all-partitions-seq
  [seq k]
  (all-partitions-seq-recur seq k '()))

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

(defn parse-into-matrix
  ([lines]
   (vec (map #(vec (map identity %)) lines)))
  ([lines fn]
   (vec (map #(vec (map fn %)) lines))))

(defn find-in-matrix
  "Finds an item from a matrix (vector of vectors) given its co-ordinates"
  [mat coords]
  (let [x (:x coords) y (:y coords)]
    (nth (nth mat y) x)))

(defn count-in-matrix
  [matrix filter-fn]
  (count (filter filter-fn (flatten matrix))))

(defn print-matrix
  [matrix]
  (printf "%s \n\n" (str/join "\n" matrix))
  matrix)

(defn update-in-matrix
  [mat coords new-value]
  (printfv "updating %s to %s\n" coords new-value)
  (let [x (:x coords) y (:y coords)]
    (assoc mat y (assoc (nth mat y) x new-value))))

(defn matrix-size
  [matrix]
  {:y (count matrix) :x (count (first matrix))})

(defn zip
  [& xs]
  (apply map vector xs))

(defn zip-and-reduce
  "zips given seqs and reduces them with the given fn"
  [fn & xs]
  (map #(reduce fn %) (apply zip xs)))

(defn valid-coords?
  [max-x max-y x y]
  (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y)))

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
    (map #(zip-and-reduce + coords %) adjustments)))

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
  [s] (Integer/parseInt (str s)))

(defn to-long
  "Converts given item into a long, first turning it into string"
  [s] (Long/parseLong (str s)))

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
