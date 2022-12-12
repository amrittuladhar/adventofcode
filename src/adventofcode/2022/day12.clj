(ns adventofcode.2022.day12)
(require '[adventofcode.util :as u]
         '[adventofcode.graph :as g]
         '[clojure.string :as str])

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day12/" file ".txt"))]
    (u/parse-into-matrix-map lines)))

(defn neighbors [[x y]]
  (for [[xx yy] '([0 1] [1 0] [0 -1] [-1 0])]
    [(+ x xx) (+ y yy)]))

(defn elevation [value]
  (cond
    (= value \S) \a
    (= value \E) \z
    :else value))

(defn reachable? [src dest]
  "src and dest are characters"
  (if (nil? dest)
    false
    (< (- (int (elevation dest)) (int (elevation src))) 2)))

(defn to-graph [height-map]
  "Converts map of coordinates to values, like {[0 0] \\a [0 1] \\c} to a graph, like {[0 0] ([0 1] [0 2])}"
  (reduce
    (fn [graph [[x y] value]]
      (let [neighbors (neighbors [x y])]
        ; (println "x y" x y ": neighbors" neighbors)
        (assoc graph [x y]
                     (filter #(reachable? value (height-map %)) neighbors))))
    {}                                                      ; initial graph
    height-map))

(defn find-char [height-map c]
  (filter (fn [[_ value]] (= value c)) height-map))

(defn find-shortest-path [height-graph src dest]
  ; (prn "src" src "dest" dest)
  (g/djikstra
    height-graph
    src
    dest
    (fn [graph node] (graph node))
    (constantly 1)))

(defn part1 [file]
  (let [height-map (parse file)
        height-graph (to-graph height-map)
        start (-> (find-char height-map \S) first first)
        end (-> (find-char height-map \E) first first)]
    (count (find-shortest-path height-graph start end))))

(defn chop-path [path node]
  "Checks if the given node occurs in the given path, and if it does, returns the rest of the path"
  ; (println "   Looking for " node "in " (if (= (count path) 0) path (count path)))
  (loop [path path]
    (let [head (first path)]
      (cond
        (empty? path) nil
        (= (first (first head)) node) (conj (rest path) head)
        :else (recur (rest path))))))

(defn find-shortest-path-of-all [height-graph starts end]
  (reduce
    (fn [existing-paths start]
      (let [existing (first (map #(chop-path % start) existing-paths))]
        ; (println "start" start "existing size" (count existing) "all paths" (count existing-paths))
        (if (nil? existing)
          (let [shortest-path (find-shortest-path height-graph start end)]
            (if (empty? shortest-path) existing-paths (conj existing-paths shortest-path)))
          (conj existing-paths existing))))
    '() ; lists of lists
    starts))

(defn part2 [file]
  (let [height-map (parse file)
        starts (->> (find-char height-map \a)
                    (concat (find-char height-map \S))
                    (map first))
        end (-> (find-char height-map \E) first first)
        height-graph (to-graph height-map)]
    (apply min (map count (find-shortest-path-of-all height-graph starts end)))))
