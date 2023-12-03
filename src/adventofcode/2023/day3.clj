(ns adventofcode.2023.day3)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn- add-to-map [map idx c-idx val]
  (into map [
             [[idx c-idx] val]
             ]))

(defn parse-line [idx line]
  (let [[map num c-idx] (reduce
                          (fn [[map num c-idx] c]
                            (let [next-idx (inc c-idx)]
                              (cond
                                (Character/isDigit (char c)) [map (str num c) next-idx]
                                (= c \.) (if (empty? num) [map "" next-idx]
                                                          [(add-to-map map (dec c-idx) idx (u/to-int num)) "" next-idx])
                                :else (if (empty? num) [(add-to-map map c-idx idx c) "" next-idx]
                                                       [(-> map
                                                            (add-to-map (dec c-idx) idx (u/to-int num))
                                                            (add-to-map c-idx idx c))
                                                        "" next-idx]))))
                          [{} "" 0]
                          line)]
    (if (empty? num) map
                     (add-to-map map (dec c-idx) idx (u/to-int num)))))

(defn parse-lines [lines]
  (reduce
    (fn [map [idx line]]
      (merge map (parse-line idx line)))
    {}
    (u/zip-indexed lines)))

(defn find-neighbors [x y len]
  (let [start-x (- x len)
        xs (range start-x (inc (inc x)))]
    (concat
      (map #(vector % (dec y)) xs)
      (map #(vector % (inc y)) xs)
      [[start-x y]]
      [[(inc x) y]])))

(defn find-neighbors-schematic [schematic [x y]]
  (let [val (str (schematic [x y]))
        len (count val)]
    ; (println "val:" val "len:" len)
    (find-neighbors x y len)))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2023/day3/" file ".txt"))
        schematic (parse-lines lines)]
    schematic))

(defn is-symbol? [schematic [x y]]
  (let [val (schematic [x y])]
    (cond
      (nil? val) false
      (number? val) false
      :else true)))

(defn is-part-number? [schematic [x y]]
  (let [neighbors (find-neighbors-schematic schematic [x y])]
    (not (empty? (filter #(is-symbol? schematic %) neighbors)))))

(defn part1 [file]
  (let [schematic (parse file)
        numbers (filter #(number? (second %)) schematic)
        part-numbers (filter #(is-part-number? schematic (first %)) numbers)]
    (reduce + (map second part-numbers))))

; Part 2 stuff

(defn part-numbers-with-possible-gears [schematic [x y]]
  (let [neighbors (find-neighbors-schematic schematic [x y])
        possible-gears (filter #(= (schematic %) \*) neighbors)]
    (if (empty? possible-gears) nil
                                [
                                 [[x y] (schematic [x y])]
                                 possible-gears
                                 ])))

(defn find-part-numbers-with-gears [possible-gears]
  (let [possible-gears-map (reduce
                             (fn [map [part possible-gears]]
                               (u/update-vals map possible-gears #(concat % [part])))
                             {}
                             possible-gears)
        gears (filter #(= (count (second %)) 2) possible-gears-map)
        part-numbers (map second gears)]
    part-numbers))

(defn gear-ratio [part-numbers]
  (->> part-numbers
       (map second)
       (reduce *)))

(defn part2 [file]
  (let [schematic (parse file)
        numbers (filter #(number? (second %)) schematic)
        possible-gears (map #(part-numbers-with-possible-gears schematic (first %)) numbers)
        part-numbers-with-gears (find-part-numbers-with-gears possible-gears)
        gear-ratios (map gear-ratio part-numbers-with-gears)]
    (reduce + gear-ratios)))