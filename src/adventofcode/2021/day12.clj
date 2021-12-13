(ns adventofcode.2021.day12)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn small? [cave]
  (= (str/lower-case cave) cave))

(defn visited? [current-path cave]
  (contains? (set current-path) cave))

(defn should-visit-part-1? [current-path cave]
  (and
    (if (small? cave) (not (visited? current-path cave)) true))) ; puzzle rules

(defn should-visit-part-2? [current-path cave]
  (cond
    (= cave "start") false
    (= cave "end") true
    (small? cave) (let [smalls (filter small? current-path)]
                    (if
                      (visited? current-path cave)
                      (= 0 (count (filter (fn [[_ times]] (= times 2)) (frequencies smalls))))
                      true))
    :else true))

; HACK - can't quite figure out how to correctly "flatten" the nested recursive solutions
; -- so
(defn split-solutions [end solutions]
  (map
    #(cons end %)
    (filter
      #(not (= (list end) %))
      (partition-by #(= % end) solutions))))

(defn find-paths [graph start end current-path should-visit-fn]
  ; (println "so far: " current-path "all next: " (graph start) "next: " (filter #(should-visit-fn current-path %) (graph start)))
  (cond
    (= (first current-path) end) (do
                                   ; (println current-path)
                                   current-path)
    :else (let [children (filter #(should-visit-fn current-path %) (graph start))]
            (split-solutions end (flatten (map #(find-paths graph % end (conj current-path %) should-visit-fn) children))))))

(defn update-graph [graph origin destination]
  (update-in
    graph
    [origin]
    (fnil (fn [existing] (conj existing destination)) '())))

(defn parse [lines]
  (loop [lines lines
         graph {}]
    (cond
      (empty? lines) graph
      :else (let [line (first lines)
                  tokens (str/split line #"-")
                  origin (tokens 0)
                  destination (tokens 1)]
              (recur
                (rest lines)
                (-> graph
                    (update-graph origin destination)
                    (update-graph destination origin)))))))

(defn solution []
  (let [data (u/read-file "./data/2021/day12/input.txt")
        caves (parse data)]
    {
     ; :part1 (count (find-paths caves "start" "end" '("start") should-visit-part-1?))
     :part2 (count (find-paths caves "start" "end" '("start") should-visit-part-2?))
     }))
