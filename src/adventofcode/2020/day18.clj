(ns adventofcode.2020.day18)
(require '[adventofcode.util :as u])

(def OPERATIONS
  {\* *
   \+ +})

(defn is-op
  [operations-map c]
  (contains? (set (keys operations-map)) c))

(defn reduce-for-op-fsa
  [operations-map expression op]
  (letfn [(reduce-by-op [accumulation]
            (reduce (operations-map op) accumulation))
          ; FSA with two states:
          ;    "no-op->" = not in the desired operation
          ;    "in-op->" = in the desired operation
          (not-op-> [[_ & rs] current-expr accumulation]
            ;(printf "no-op-> head: %s current-expr: %s acc: %s\n"
            ;        _ (vec current-expr) (vec accumulation))
            (cond
              ; matching operation
              (= _ op) (in-op-> rs current-expr accumulation)
              ; non-matching operation
              (is-op operations-map _) (not-op-> rs (concat current-expr accumulation (list _)) nil)
              ; number
              (int? _) (not-op-> rs current-expr (concat accumulation (list _)))
              (nil? _) (concat current-expr accumulation)
              :else (throw (IllegalArgumentException. (format "Invalid term '%s'" _)))))
          (in-op-> [[_ & rs] current-expr accumulation]
            ;(printf "in-op-> head: %s current-expr: %s acc: %s\n"
            ;        _ (vec current-expr) (vec accumulation))
            (cond
              ; matching operation
              (= _ op) (in-op-> rs current-expr accumulation)
              ; non-matching operation
              (is-op operations-map _) (not-op-> rs (concat current-expr (list (reduce-by-op accumulation)) (list _)) nil)
              ; number
              (int? _) (in-op-> rs current-expr (concat accumulation (list _)))
              (nil? _) (concat current-expr (list (reduce-by-op accumulation)))
              :else (throw (IllegalArgumentException. (format "Invalid term '%s'" _)))))]
    (trampoline not-op-> expression '() '())))

(defn reduce-with-precedence
  "precedence is sequence of operation characters in order"
  [precedence operations-map expression]
 (let [reduced (reduce #(reduce-for-op-fsa operations-map %1 %2) expression precedence)]
   (if (= (count reduced) 1)
     (first reduced)
     (throw (IllegalStateException. (format "Could not reduce expression '%s' using precedence '%s'" expression precedence))))))

(defn reduce-left-to-right
  [operations-map expression]
  ;(printf "reduce: %s\n" expression)
  (:acc (reduce
    (fn [acc term]
      (cond
        (is-op operations-map term) (assoc acc :op (operations-map term))
        :else (let [current-op (:op acc)
                    current-value (:acc acc)]
                (assoc acc :acc (current-op current-value term)))))
    {:acc 0 :op (operations-map \+)}
    expression)))

(defn term
  [c]
  (cond
    (Character/isDigit c) (u/to-int c)
    :else c))

(defn calculate
  [operations-map reduce-fn expression]
  (loop [expr expression
         stack '()]
    ;(printf "calculate-2: stack: %s\n" stack)
    (if (empty? expr)
      (reduce-fn operations-map (reverse stack))
      (let [head (first expr) tail (rest expr)]
        (cond
          (= head \)) (let [sub-expr (reverse (for [term stack :while (not= term \()] term))]
                        ; (printf "  sub-expr: %s" sub-expr)
                        (if (empty? sub-expr)
                          (throw (IllegalStateException. "Non-matching parens"))
                          ; calculate and replace expression with value, making sure to remove the (
                          (let [sub-expr-size (count sub-expr)
                                sub-expr-value (reduce-fn operations-map sub-expr)
                                new-stack (nth (iterate pop stack) (inc sub-expr-size))]
                            ;(printf "sub-expr: size: %s value: %s new-stack: %s\n"
                            ;        sub-expr-size sub-expr-value new-stack)
                            (recur tail (conj new-stack sub-expr-value)))))
          :else (recur tail (conj stack (term head))))))))

(defn remove-spaces
  [expr]
  (filter #(not (= \  %)) expr))

(def data (u/read-file "./data/day/18/input.txt"))

(defn solve
  ([data]
   {:part1 (reduce + (map #(calculate OPERATIONS reduce-left-to-right (remove-spaces %)) data))
    :part2 (reduce + (map #(calculate OPERATIONS (partial reduce-with-precedence '(\+ \*)) (remove-spaces %)) data))})
  ([]
   (solve data)))