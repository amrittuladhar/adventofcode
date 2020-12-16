(ns adventofcode.2020.day10)
(require '[adventofcode.util :as u])

(def data-smallest
  [16
   10
   15
   5
   1
   11
   7
   19
   6
   12
   4])
(def data-small
  [28
   33
   18
   42
   31
   14
   46
   20
   48
   47
   24
   23
   49
   45
   19
   38
   39
   11
   1
   32
   25
   35
   8
   17
   7
   9
   4
   2
   34
   10
   3])

(defn calculate-diffs
  [jolts]
  (map #(- (second %) (first %)) (u/partition-seq (sort jolts) 2)))

(defn build-graph
  ; e.g. jolts: (1 3 4 6 7 9), increments: (1 3) yields (1 -> (4), 3 -> (4 6), 4 -> (7), 6 -> (7 9))
  ([all-jolts-set jolts graph increments]
   (let [first-jolt (first jolts) rest-jolts (rest jolts)]
     (if (empty? jolts)
       graph
       (build-graph all-jolts-set rest-jolts
                    (assoc graph first-jolt
                                 (filter #(contains? all-jolts-set %) (map (partial + first-jolt) increments)))
                    increments))))
  ([jolts]
   (build-graph (set jolts) jolts {} '(1 2 3))))

(defn find-possible-paths
  ([graph current-jolt target paths current-path]
   (cond
     (= current-jolt target) (conj paths current-path)
     (> current-jolt target) paths
     :else (map #(find-possible-paths graph % target paths (cons current-jolt current-path)) (graph current-jolt))))
  ([graph target]
   (find-possible-paths graph 0 target '() '())))

(defn count-reduce
  [memo-sum1 memo-sum2]
  ; (printf "reduce: 1: %s 2: %s\n" memo-sum1 memo-sum2)
  [(+ (first memo-sum1) (first memo-sum2)) (merge (second memo-sum1) (second memo-sum2))])

(defn count-possible-paths
  ([graph current-jolt target level current-count memo]
   ;(printf "current: %s count: %s\n", current-jolt current-count)
   (cond
     (not (nil? (memo current-jolt))) (memo current-jolt)
     (= current-jolt target) (let [new-count (inc current-count)]
                               ;(printf "   inc count from: %s\n" current-count)
                               new-count)
     (> current-jolt target) current-count
     :else (let [next-jolts (graph current-jolt) memo {}]
             ;(printf "   current: %s next: %s\n" current-jolt (vec next-jolts))
             (first (reduce count-reduce
                     (map #(if
                             (nil? (memo %))
                             (let [new-value (count-possible-paths graph % target level current-count memo)]
                               [new-value (assoc memo % new-value)])
                             [(memo %) memo])
                          next-jolts))))))
  ;(reduce + (map #(let [r (count-possible-paths graph % target level current-count)]
  ;        ; (printf "   result from cur %s: %s\n" % current-jolt r)
  ;        r)
  ;     next-jolts)))))
  ([graph target]
   (count-possible-paths graph 0 target 1 0 {})))

;(defn count-possible-paths
;  ([jolts-set current-jolt target level]
;   (printf "%s current-jolt: %s target: %s\n" (u/indent level) current-jolt target)
;   (cond
;     (and (not (= current-jolt 0)) (not (contains? jolts-set current-jolt))) 0
;     (> current-jolt target) 0
;     (= current-jolt target) 1
;     :else (+
;             (count-possible-paths jolts-set (+ current-jolt 1) target (* 2 level))
;             (count-possible-paths jolts-set (+ current-jolt 3) target (* 2 level)))))
;  ([jolts]
;   (let [max-jolt (reduce max jolts)]
;     (count-possible-paths (set jolts) 0 (+ 3 max-jolt) 1))))

(def data (u/read-file "./data/day/10/input.txt"))

(defn solution
  []
  (let [jolts (map u/to-long data)
        diffs (calculate-diffs jolts)
        diffs-1 (filter #(= % 1) diffs)
        diffs-3 (filter #(= % 3) diffs)
        max-jolt (reduce max jolts)
        device-jolt (+ max-jolt 3)
        all-jolts (conj jolts 0 device-jolt)
        graph (build-graph all-jolts)]
    {:part1 (* (inc (count diffs-1)) (inc (count diffs-3)))
     :part2 (count-possible-paths graph device-jolt)}))
