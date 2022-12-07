(ns adventofcode.2022.day7)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn apply-cd [command current]
  (let [dir (subs command (count "$ cd "))]
    (cond
      (= dir "..") (pop current)
      :else (conj current dir))))

(defn parse-file [file-spec]
  (let [tokens (str/split file-spec #" ")]
    {
     :name (second tokens)
     :size (u/to-int (first tokens))
     }))

(defn apply-sizes [tree current {_ :name size :size}]
  (loop [tree tree current current]
    ; (println "Applying " name size " to " current " " tree)
    (if (empty? current)
      tree
      (recur (update-in tree [current] (partial + size))
             (pop current)))))

(defn parse-lines [lines]
  (loop [lines lines
         tree {}
         current '()]
    (let [head (first lines) tail (rest lines)]
      ; (println "head" head)
      (cond
        (empty? lines) tree
        (str/starts-with? head "$ cd ") (let [new-dir (apply-cd head current)]
                                          (recur tail (merge {new-dir 0} tree) new-dir))
        (u/regex-match? #"^\d.*" head) (let [file (parse-file head)]
                                         (recur tail (apply-sizes tree current file) current))
        ; we can ignore "ls" and "dir" names
        :else (recur tail tree current)))))

(defn without-root [tree]
  (filter (fn [[dir _]] (not (= dir '("/")))) tree))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day7/" file ".txt"))]
    (parse-lines lines)))

(defn part1 [file]
  (let [tree (parse file)]
    (->> tree
         without-root
         (filter (fn [[_ size]] (<= size 100000)))
         (map second)
         (reduce +))))

(defn part2 [file]
  (let [tree (parse file)
        total-size (tree '("/"))
        free-space (- 70000000 total-size)
        diff (- 30000000 free-space)]
    (->> tree
         (filter (fn [[_ size]] (>= size diff)))
         (map second)
         (apply min))))

