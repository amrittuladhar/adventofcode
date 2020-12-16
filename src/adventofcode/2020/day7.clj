(ns adventofcode.2020.day7)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-bag
  [bag]
  (first (str/split bag #" bags*.*")))

(defn parse-containee-bag
  [containee-bag]
  (let [tokens (str/split containee-bag #" " 2)
        bag-count (first tokens)
        bag (parse-bag (second tokens))]
    (if (= bag-count "no")
      []
      [bag (u/to-int bag-count)])))

(defn parse-line
  [line]
  (let [tokens (str/split line #" contain ") ; "red bags contain 3 green bags, 4 blue bags" -> [ "red bags", "3 green bags, 4 blue bags" ]
        container-bag (parse-bag (first tokens)) ; "red"
        containees-all (second tokens) ; "3 green bags, 4 blue bags"
        containees (str/split containees-all #", ") ; [ "3 green bags, 4 blue bags" ]
        containee-bags (map parse-containee-bag containees) ; ( ["green", 3], ["blue", 4] )
        ]
    (map #(assoc {} :container container-bag :containee (first %) :count (second %)) containee-bags)))

(defn collect-bags-recur
  [lines collected]
  (if (empty? lines)
    collected
    (collect-bags-recur (rest lines) (concat collected (flatten (first lines))))))

(defn collect-bags
  [lines]
  (collect-bags-recur lines nil))

(defn find-possible-containers-recur
  [bags-map bag current-containers]
  (let [container-bags (bags-map bag)]
    (if (empty? container-bags)
      current-containers
      (let [all-containers (map :container container-bags)]
        (map concat (map #(find-possible-containers-recur bags-map % (cons % current-containers)) all-containers))))))

(defn find-possible-containers
  [bags-map bag]
  (find-possible-containers-recur bags-map bag nil))

; ====== this function is way more complicated than it needs to be, probably
(defn count-containees-recur
  [bags-map bag current-count level]
  (let [containee-bags (bags-map bag)]
    (if (or (empty? containee-bags) (nil? (:containee (first containee-bags))))
      0
      (let [non-empty-containees (filter #(not (nil? (:containee %))) containee-bags)]
        (reduce + 0 (map #(let [containee (:containee %) cnt (:count %)]
                            (printf "%s containee: '%s' current-count: %s count: %s non-empty-containees:%s\n"
                                    (str/join (repeat level " ")) containee current-count cnt (vec non-empty-containees))
                            (+ cnt (* cnt (count-containees-recur bags-map containee 1 (* level 4))))) non-empty-containees))))))

(defn count-containees
  [bags-map bag]
  (count-containees-recur bags-map bag 1 1))

(def data (u/read-file "./data/day/7/input.txt"))

(def data-small (u/read-file "./data/day/7/input-small.txt"))
(def data-small-2 (u/read-file "./data/day/7/input-small-2.txt"))

(defn solution []
  (let [bags (map parse-line data)
        collected-bags (collect-bags bags)
        bags-map-by-containee (group-by :containee collected-bags)
        bags-map-by-container (group-by :container collected-bags)]
    {:part1 (count (set (flatten (find-possible-containers bags-map-by-containee "shiny gold"))))
     :part2 (count-containees bags-map-by-container "shiny gold")}))

