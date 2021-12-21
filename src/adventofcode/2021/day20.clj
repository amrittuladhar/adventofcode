(ns adventofcode.2021.day20)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def LIGHT \#)
(def DARK \.)

(defn expand [image units]
  (let [visible (:visible image)
        invisible (:invisible image)
        original-max-x (apply max (map first (keys visible)))
        original-max-y (apply max (map second (keys visible)))
        new-max-x (+ original-max-x (* 2 units))
        new-max-y (+ original-max-y (* 2 units))
        empty-expanded (u/pairs-to-map
                         (for [x (range (inc new-max-x)) y (range (inc new-max-y))]
                           [[x y] invisible]))]
    (assoc image :visible (reduce
                            (fn [new-image [[x y] pixel]]
                              (assoc new-image [(+ x units) (+ y units)] pixel))
                            empty-expanded
                            visible))))

(defn pixel-to-binary [pixel]
  ; (println "mapping:" pixel)
  (if (= pixel LIGHT) "1" "0"))

(defn map-to-algo [algo binary]
  ; (println "binary:" binary)
  (nth algo (Integer/parseInt binary 2)))

(defn map-pixels-to-algo [algo pixels]
  (->> pixels
       (map pixel-to-binary)
       str/join
       (map-to-algo algo)))

(defn get-pixel [image coords]
  (u/if-nil? ((:visible image) coords) (:invisible image)))

(defn calculate-new-pixel [algo image neighbors]
  (->> (map #(get-pixel image %) neighbors)
       (map-pixels-to-algo algo)))

(defn find-inset [x y]
  (for [yy (range (dec y) (+ y 2))
        xx (range (dec x) (+ x 2))]
    ; :when (and (>= xx 0) (>= yy 0))]
    [xx yy]))

(defn find-new-value [algo image x y]
  (let [inset (find-inset x y)
        new-value (calculate-new-pixel algo image inset)]
    ;new-value (if (= (count inset) 9)
    ;            (calculate-new-pixel algo image inset)
    ;            current]
    ; (println "inset:" inset)
    new-value))

(defn enhance [algo image]
  ; (println "invisible:" (:invisible image))
  {:visible   (reduce-kv
                (fn [m [x y] _]
                  (assoc m [x y] (find-new-value algo image x y)))
                {}
                (:visible image))
   :invisible (->> (repeat 9 (:invisible image))
                   (map-pixels-to-algo algo))})

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day20/" file ".txt"))]
    {
     :algo  (first lines)
     :image {:visible   (u/parse-into-matrix-map (rest (rest lines)))
             :invisible DARK}
     }
    ))

(defn filter-image [image value]
  ; (println "filter-image:" image)
  (filter (fn [[_ v]] (= v value)) (:visible image)))

(defn process [image algo times]
  (reduce
    (fn [image _]
      ; (u/print-matrix-map (:visible image))
      (enhance algo (expand image 2)))
    image
    (range 1 (inc times))))

(defn part1
  ([file]
   (part1 file 2))
  ([file times]
   (let [parsed (parse file)
         image (:image parsed)
         algo (:algo parsed)]
     (-> (process image algo times)
         (filter-image LIGHT)
         count))))

(defn part2 [file]
  (part1 file 50))
