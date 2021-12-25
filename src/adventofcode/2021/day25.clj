(ns adventofcode.2021.day25)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def BLANK \.)
(def EAST \>)
(def SOUTH \v)

(defn move [grid position
            x-fn y-fn wrap-fn
            herd]
  ; (println "move:" "position:" position "herd:" herd "value:" (grid position))
  (if (= (grid position) herd)
    (let [[x y] position
          [new-x new-y] [(x-fn x) (y-fn y)]]
      (let [[new-x new-y] (if (grid [new-x new-y]) [new-x new-y] (wrap-fn position))
            new-value (grid [new-x new-y])]
        ; (println "move:" herd [x y] "->" [new-x new-y] "value:" (grid [x y]) "next:" new-value)
        (if (or (nil? new-value) (= new-value BLANK))
          {[x y] BLANK [new-x new-y] herd}
          {position herd})))
    {position (grid position)}))

(defn move-east [grid [x y]]
  ; (println "move-east: position:" [x y] "grid:" (take 3 grid))
  (move
    grid
    [x y]
    inc identity (fn [_] [0 y])
    EAST))

(defn move-south [grid [x y]]
  ; (println "move-south: position:" [x y] "grid:" (take 3 grid))
  (move
    grid
    [x y]
    identity inc (fn [_] [x 0])
    SOUTH))

(defn move-grid [grid move-fn]
  (:acc (reduce
          (fn [{updated :updated acc :acc} [p _]]
            (if (contains? updated p)
              {
               :updated updated
               :acc     acc
               }
              (let [moved (move-fn grid p)
                    keys (keys moved)]
                ; (println "   updated:" moved)
                {
                 :updated (apply conj updated keys)
                 :acc     (merge acc moved)
                 }
                )))
          {
           :updated #{}
           :acc     {}
           }
          grid)))

(defn run-step [grid]
  (-> grid
      (move-grid move-east)
      (move-grid move-south)))

(defn run-until-no-change [grid]
  (first
    (filter
      (fn [part]
        (let [prev (first part)
              current (second part)]
          (= (first prev) (first current))))
      (partition
        2 1
        (u/zip
          (iterate run-step grid)
          (iterate inc 0))))))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day25/" file ".txt"))]
    (u/parse-into-matrix-map lines)))

(defn part1 [file]
  (let [herds (parse file)]
    (second (second (run-until-no-change herds)))))

(defn part2 [file]
  (let [data (parse file)]
    data))    
