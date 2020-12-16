(ns adventofcode.2020.day9)
(require '[adventofcode.util :as u])

(defn find-summing-numbers-in-partition
  [partition num-count]
  (let [size (count partition)
        sum (last partition)
        nums (take (dec size) partition)]
    (u/find-summing-numbers nums sum num-count)))

(defn find-non-summing-partitions
  [preamble-length all-data sum-count]
  (let [partitions (u/partition-seq all-data (inc preamble-length))]
    (filter #(empty? (find-summing-numbers-in-partition % sum-count)) partitions)))

(defn find-non-sums
  [preamble-length all-data sum-count]
  (map last (find-non-summing-partitions preamble-length all-data sum-count)))

(defn find-contiguous-summing-numbers
  ([vec sum start-index end-index]
    (let [sub-vec (subvec vec start-index end-index)
          sub-sum (reduce + sub-vec)]
      (cond
        (>= end-index (count vec)) nil
        (= sub-sum sum) sub-vec
        (> sub-sum sum) (find-contiguous-summing-numbers vec sum (inc start-index) end-index)
        :else (find-contiguous-summing-numbers vec sum start-index (inc end-index)))))
    ([vec sum]
     (find-contiguous-summing-numbers vec sum 0 1)))

(defn parse
  [lines preamble-length]
  {:all lines :preamble (take preamble-length lines) :data (drop preamble-length lines)})

(def data (u/read-file "./data/day/9/input.txt"))

(defn solution []
  (let [preamble-length 25
        parsed (parse (map u/to-long data) preamble-length)
        all-data (:all parsed)]
    (def non-sum (first (find-non-sums preamble-length all-data 2)))
    (def summing-nums (find-contiguous-summing-numbers (vec all-data) non-sum))
    {:part1 non-sum
     :summing-nums summing-nums
     :sum (reduce + summing-nums)
     :part2 (+ (reduce min summing-nums) (reduce max summing-nums))}))