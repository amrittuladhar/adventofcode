(ns adventofcode.2023.day5)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-range [range-line]
  (let [nums (->> (str/split range-line #" ")
                  (map str/trim)
                  (map u/to-long))]
    {:dest-range   (first nums)
     :src-range    (second nums)
     :range-length (last nums)}))

(defn map-to-dest-2 [{dest-range :dest-range src-range :src-range range-length :range-length} val]
  (let [diff (- val src-range)]
    (if
      (and (>= val src-range) (<= val (+ src-range range-length)))
      (+ dest-range diff)
      nil)))

(defn map-to-dest [range-map src dest val]
  (let [ranges (range-map [src dest])
        mapped-vals (map #(map-to-dest-2 % val) ranges)]
    (as-> mapped-vals data
          (filter #(not (nil? %)) data)
          (first data)
          (u/if-nil? data val))))

(defn parse-map-string [map-line]
  (let [tokens (str/split map-line #" ")
        src-dest (str/split (first tokens) #"\-to\-")]
    {:src (first src-dest) :dest (second src-dest)}))

(defn parse-lines-1 [lines]
  (reduce
    (fn [{range-map :map seeds :seeds src :src dest :dest} line-full]
      (let [line (str/trim line-full)]
        (cond
          (str/starts-with? line "seeds:") (let [seed-string (str/trim (subs line 6))]
                                             {
                                              :map   {}
                                              :seeds (map u/to-long (str/split seed-string #" "))
                                              :src   src
                                              :dest  dest
                                              }
                                             )
          (str/ends-with? line "map:") (let [parsed (parse-map-string line)]
                                         {:seeds seeds :map range-map :src (:src parsed) :dest (:dest parsed)})
          (empty? line) {:seeds seeds :map range-map :src nil :dest nil}
          :else (let [range (parse-range line)]
                  {
                   :seeds seeds
                   :map   (update range-map [src dest] #(conj % range))
                   :src   src
                   :dest  dest
                   }))))
    {:seeds '() :map {} :src nil :dest nil}
    lines))

(defn find-location [seed ranges-map]
  (loop [src "seed"
         val seed
         path []]
    (let [dest (->> ranges-map
                 (filter (fn [[[s _] _]] (= s src)))
                 first first second)
          mapped (map-to-dest ranges-map src dest val)
          new-path (conj path [src dest val mapped])]
      ;(println "src" src "dest" dest "val" val "mapped" mapped "path" path "new-path" new-path)
      (if
        (= dest "location") new-path (recur dest mapped new-path)))))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2023/day5/" file ".txt"))]
    (parse-lines-1 lines)))

(defn part1 [file]
  (let [{seeds :seeds ranges-map :map} (parse file)
        paths (map #(find-location % ranges-map) seeds)
        locations (map #(last (last %)) paths)]
    (apply min locations)))

; Part 2 stuff

;(def MEMO {})
;
;(def DESTS
;  {
;   "seed" "soil"
;   "soil" "fertilizer"
;   "fertilizer" "water"
;   "water" "light"
;   "light" "temperature"
;   "temperature" "humidity"
;   "humidity" "location"
;   })
;(defn find-location-2 [seed ranges-map]
;  (loop [src "seed"
;         val seed]
;    (let [dest (DESTS src)
;          memo (MEMO [src val])]
;      (if (nil? memo)
;        (let [mapped (map-to-dest ranges-map src dest val)]
;          ()
;      ;(println "src" src "dest" dest "val" val "mapped" mapped "path" path "new-path" new-path)
;      (if
;        (= dest "location") mapped (recur dest mapped)))))
;
;(defn expand-seeds [seeds]
;  (->> (partition 2 2 seeds)
;       (map #(range (first %) (inc (+ (first %) (second %)))))
;       flatten))
;
;(defn part2 [file]
;  (let [{seeds :seeds ranges-map :map} (parse file)
;        expanded (expand-seeds seeds)
;        locations (map #(find-location-2 % ranges-map) expanded)]
;    (apply min locations)))
