(ns adventofcode.2022.day10)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn addx [value registers]
  (update-in registers ["X"] (fn [{_ :during to :after}] {:during to :after (+ to value)})))

(defn noop [last-state]
  "Makes a copy of the last state the corrects the value of registers at the beginning of the new state"
  (-> (map
        (fn [[register {_ :during to :after}]] [register {:during to :after to}])
        last-state)
      u/pairs-to-map))

(defn parse-command [line]
  (cond
    (= line "noop") {:update-fn noop :cost 1 :name "noop"}
    :else (let [[command value] (str/split line #" ")]
            (cond
              (= command "addx") {:update-fn (partial addx (u/to-int value)) :cost 2 :name line}))))

(defn execute [commands initial]
  (reduce
    (fn [states command]
      (let [{update-fn :update-fn cost :cost} command
            last-state (last states)]
        (vec (concat states
                     (repeat (dec cost) (noop last-state))
                     (list (update-fn last-state))))))
    [initial]
    commands))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2022/day10/" file ".txt"))]
    (map parse-command lines)))

(defn register-value-during [state register]
  (:during (state register)))

(defn part1 [file]
  (let [commands (parse file)]
    (as-> (execute commands {"X" {:during 1 :after 1}}) execution
          (map #(vector
                  %
                  (register-value-during (execution %) "X"))
               '(20 60 100 140 180 220))
          (map #(*
                  (first %)
                  (second %))
               execution)
          (reduce + execution))))

; Part 2
(defn is-lit? [pixel x-value]
  (<= (Math/abs (- pixel x-value)) 1))

(defn part2 [file]
  (let [commands (parse file)
        executions (execute commands {"X" {:during 1 :after 1}})
        rows (partition 40 40 (rest executions))]
    (->> (for [row rows]
           (-> (for [[pixel {{during :during _ :after} "X"}] (u/zip-indexed row)]
                 (if (is-lit? pixel during) "#" "."))
               str/join
               ))
         (str/join "\n"))))
