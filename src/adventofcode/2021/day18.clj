(ns adventofcode.2021.day18)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn is-pair? [v]
  (and (vector? v) (number? (first v)) (number? (second v))))

(defn magnitude
  ([num]
   (cond
     (number? num) num
     :else (+ (* 3 (magnitude (first num))) (* 2 (magnitude (second num)))))))

(defn add
  ([[num1 num2]]
   (add num1 num2))
  ([num1 num2]
   [num1 num2]))

(defn possible-addresses []
  (map
    (fn [s] (map #(Integer/parseInt (str %)) s))
    (map #(u/pad-left (Integer/toBinaryString %) 4 "0") (range 16))))

(defn find-fourth-pair [num]
  (first
    (filter #(is-pair? (:value %))
            (keep
              identity
              (map
                #(u/pairs-to-map [[:address %] [:value (get-in num %)]])
                (possible-addresses))))))

(defn make-new-address [parent branch]
  (if (nil? parent) [] (conj parent branch)))

(defn make-addressed-tree [tree]
  (let [walk (fn walk [node parent branch]
               (let [address (make-new-address parent branch)]
                 ; (println "walk:" "node:" node "parent:" parent "branch:" branch)
                 (lazy-seq
                   (cons {:address address :value node}
                         (when (vector? node)
                           (mapcat walk node (repeat (count node) address) '(0 1)))))))]
    (walk tree nil 0)))

(defn find-explode-updates [num addressed-tree]
  (let [fourth-pair (find-fourth-pair num)
        address4th (:address fourth-pair)
        value4th (:value fourth-pair)]
    (cond
      fourth-pair (loop [nodes addressed-tree
                         left-num nil
                         right-num nil
                         explode-pair-passed? false]
                    (cond
                      ; end reached, return updates to be made to the tree
                      (empty? nodes) (let [left (first value4th)
                                           right (second value4th)]
                                       ;(println explode-pair "explode-pair" explode-pair-value "left" left-num "right" right-num)
                                       (filter some?
                                               (list
                                                 (if left-num {:old-value (:value left-num) :address (:address left-num) :value (+ (:value left-num) left)})
                                                 (if right-num {:old-value (:value right-num) :address (:address right-num) :value (+ (:value right-num) right)})
                                                 {:old-value value4th :address address4th :value 0}
                                                 )))
                      :else (let [node (first nodes) value (:value node) address (:address node)]
                              ; (println "value:" value "left:" left-num "passed?" explode-pair-passed? "right:" right-num "nodes:" nodes)
                              (cond
                                (number? value) (if explode-pair-passed?
                                                  ; explode-pair passed, so we're looking for the right side now, and we're done
                                                  (recur nil left-num node explode-pair-passed?)
                                                  ; not passed yet, so update the "left side" value
                                                  (recur (rest nodes) node right-num explode-pair-passed?))
                                ; we're at the leaf corresponding to the pair to explode
                                ; we'll skip the next two nodes to look for the right side
                                ; and also replace explode-pair with the current "addressed" value
                                (= address address4th) (recur (drop 3 nodes) left-num right-num true)
                                :else (do
                                        ; (println "Vec found nodes" nodes)
                                        (recur (rest nodes) left-num right-num explode-pair-passed?))))))
      :else '())))

; returns {:exploded true/false :value num}
(defn explode-once [num addressed-tree]
  (let [explosion (find-explode-updates num addressed-tree)]
    (cond
      (empty? explosion) {:exploded false :value num}
      :else {:exploded true
             :value    (reduce
                         (fn [num update]
                           ; (println "Updating" (:address update) (get-in num (:address update)) "to" (:value update))
                           (update-in num (:address update) (constantly (:value update))))
                         num
                         explosion)})))

(defn find-split-update [addressed-tree]
  (let [to-split (first (filter
                          (fn [{address :address value :value}]
                            ; (println "finding split:" address value)
                            (and
                              (number? value)
                              (>= value 10)))
                          addressed-tree))]
    (when (not (nil? to-split))
      {:address (:address to-split)
       :value   (u/quotient-pair (:value to-split) 2)})))

(defn split-once [num addressed-tree]
  (let [split-update (find-split-update addressed-tree)]
    ; (println "split-update:" split-update)
    (if
      (nil? split-update) {:split false :value num}
                          {:split true :value (update-in num (:address split-update) (constantly (:value split-update)))})))

(defn split [num]
  (loop [prev nil
         num num]
    (cond
      (= prev num) num
      :else (let [split-up (split num)]
              ; (println "after split:" split-up)
              (recur num split-up)))))

(defn reduce-num [num]
  (loop [num num]
    (let [addressed-tree (make-addressed-tree num)
          explode-result (explode-once num addressed-tree)]
      ; (println "after explode:" num)
      (if (not (:exploded explode-result))
        (let [split-result (split-once num addressed-tree)]
          ; (println "after split:" split-result)
          (if (not (:split split-result))
            num
            (recur (:value split-result))))
        (recur (:value explode-result))))))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day18/" file ".txt"))]
    (map read-string lines)))

(defn part1 [file]
  (let [data (parse file)]
    (magnitude (reduce #(reduce-num (add %1 %2)) data))))

(defn part2 [file]
  (let [data (parse file)
        product (keep identity (for [a data b data] [a b]))]
    (apply max (map magnitude
                    (map reduce-num
                         (map #(add (first %) (second %))
                              product))))))


; FINALLY UNUSED BUT INTERESTING

(defn make-tree [num]
  ; (println "num" num)
  (cond
    (not (vector? num)) (vector num)
    :else (let [head (first num)
                tail (subvec num 1)]
            (if (empty? tail)
              (make-tree head)
              (conj [] (make-tree head) (make-tree tail))))))

(defn make-addressed-tree-old [tree]
  (loop [branch 0
         addressed-tree []
         prefixes []
         visited-set #{}]
    (let [address (conj (u/if-nil? (peek prefixes) []) branch)
          value (get-in tree address)]
      (println "branch:" branch "address:" address "value:" value "prefixes:" prefixes)
      (cond
        (contains? visited-set address) (if (empty? prefixes)
                                          (case branch
                                            1 addressed-tree
                                            0 (recur 1 addressed-tree [] visited-set))
                                          (recur 0 addressed-tree (pop prefixes) visited-set))
        (number? value) (if (= branch 0)
                          (recur 1 (conj addressed-tree {:address address :value value}) prefixes (conj visited-set address))
                          (if (empty? prefixes)
                            (conj addressed-tree {:address address :value value})
                            (recur 0 (conj addressed-tree {:address address :value value}) (pop prefixes) (conj visited-set address))))
        (not (nil? value)) (recur 0 (conj addressed-tree {:address address :value value}) (conj prefixes address) (conj visited-set address))
        (nil? value) (recur 0 addressed-tree (pop prefixes) visited-set)
        (empty? prefixes) addressed-tree))))

; current = current address
; node = next node
; branch = 0 (left), or 1 (right)
; returns {:address node-address :branch next-branch :next-prefix next-prefix}
(defn update-address [prefixes node branch]
  (println "update-address:" "node:" node "existing-prefixes:" prefixes "branch" branch)
  (let [address (conj (u/if-nil? (last prefixes) []) branch)]
    {
     :address  address
     :branch   (if (number? node) 1 0)                      ; switch branch
     ; :prefixes (if (= branch 0) prefixes (pop prefixes))
     :prefixes (if (number? node)
                 (if (= branch 0) prefixes (pop prefixes))  ; if left, no need to pop prefix
                 (conj prefixes address))}))

(defn address-tree [tree]
  (loop [nodes tree
         prefixes []
         branch 0
         addressed-tree []]
    (cond
      (empty? nodes) addressed-tree
      :else (let [node (first nodes)
                  new-address (update-address prefixes node branch)]
              ; (println "address-tree:" "node" node "result" new-address)
              (recur (rest nodes)
                     (:prefixes new-address)
                     (:branch new-address)
                     ; new address is prefixed by an extra 0 because of the root node
                     (conj addressed-tree {:address (rest (:address new-address)) :value node}))))))

(defn left-most-fourth-pair [num]
  (first
    (filter is-pair?
            (keep
              identity
              (map
                #(get-in num %)
                (possible-addresses))))))

(defn tree [num]
  (tree-seq vector? identity num))
