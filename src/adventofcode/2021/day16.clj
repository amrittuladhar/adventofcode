(ns adventofcode.2021.day16)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(def VERSION-LENGTH 3)
(def TYPE-ID-LENGTH 3)
(def HEADER-LENGTH (reduce + (list VERSION-LENGTH TYPE-ID-LENGTH)))

(defn to-string [v]
  (if (seq? v) (str/join v) (str v)))

(defn to-binary-str [hex]
  ; (println "to-binary: hex:" hex)
  (reduce
    (fn [binary hex-single]
      (str binary (u/pad-left (Integer/toBinaryString (Integer/parseInt (str hex-single) 16)) 4 "0")))
    ""
    hex))

(defn to-decimal [binary]
  (Long/parseLong (to-string binary) 2))

(defn process
  ([packet n f]
   [(f (take n packet)) (drop n packet)])
  ([packet n]
   (process packet n identity)))

(defn drop-header [packet]
  (let [all-binary packet]
    (drop HEADER-LENGTH all-binary)))

(defn read-header [packet]
  (let [header (take HEADER-LENGTH packet)
        [version rest] (process header VERSION-LENGTH to-decimal)
        [type-id _] (process rest TYPE-ID-LENGTH to-decimal)]
    ; (println "header:" header "version:" version "type-id:" type-id)
    [version type-id]))

(declare parse-packet)
(declare parse-packets-by-length)
(declare parse-packets-by-count)

(defn parse-literal [version type-id packet group-length]
  (let [packet-binary (drop HEADER-LENGTH packet)
        groups (->> packet-binary
                    (partition group-length group-length)
                    (u/take-upto (fn [part] (= (first part) \0))))
        result (->> groups
                    (map rest)
                    flatten
                    to-decimal)
        groups-length (reduce + (map count groups))
        consumed-length (+ groups-length HEADER-LENGTH)
        remaining (drop consumed-length packet)]
    ; (println "literal:" "version:" version "type-id:" type-id "value:" result "next:" remaining)
    ; (println "groups-length:" groups-length "consumed:" consumed)
    {
     :version  version
     :type     :literal
     :type-id  type-id
     :value    result
     :consumed consumed-length
     :next     remaining
     :children '()
     }))

(defn parse-operator [version type-id packet]
  (let [binary (drop-header packet)
        [length-type-id rest] (process binary 1 first)
        length-length (case length-type-id \0 15 \1 11)
        [length rest] (process rest length-length to-decimal)
        children (case length-type-id
                            \0 (parse-packets-by-length rest length)
                            \1 (parse-packets-by-count rest length))
        consumed (+ HEADER-LENGTH 1 length-length (reduce + (map :consumed children)))]

    ; (println "binary:" binary "length-type-id:" length-type-id)
    ; (println "operator:" length-type-id "version:" version "type-id:" type-id "value:" type-id "next:" '())
    {
     :version        version
     :type           :operator
     :type-id        type-id
     :value          type-id
     :consumed       consumed
     :length-type-id length-type-id
     :length         length
     :children       children
     :next           (drop consumed packet)
     }))

(defn parse-packets-by-count [bits total-count]
  (loop [packets '()
         bits bits]
    ; (println "parsing by count: so far:" (count packets) "total:" total-count "remaining bits" (count bits))
    (cond
      (= (count packets) total-count) packets
      :else (let [[version type-id] (read-header bits)]
              (case type-id
                4 (let [packet (parse-literal version type-id bits 5)]
                    (recur (conj packets packet) (:next packet)))
                (let [packet (parse-operator version type-id bits)]
                  (recur (conj packets packet) (:next packet))))))))

(defn parse-packets-by-length [bits total-length]
  (loop [packets '()
         bits bits
         consumed 0]
    (cond
      (= consumed total-length) packets
      (empty? bits) packets
      :else (let [[version type-id] (read-header bits)]
              (case type-id
                4 (let [packet (parse-literal version type-id bits 5)]
                    (recur (conj packets packet) (:next packet) (+ consumed (:consumed packet))))
                ; default
                (let [packet (parse-operator version type-id bits)]
                  (recur (conj packets packet) (:next packet) (+ consumed (:consumed packet)))))))))


; { :version version :value value :type literal/op }
(defn parse-packet [packet]
  (let [[version type-id] (read-header packet)]
    (cond
      (= type-id 4) (parse-literal version type-id packet 5)
      :else (parse-operator version type-id packet))))

(defn add-versions [packet]
  (+ (:version packet) (reduce + (map add-versions (:children packet)))))

(defn parse [file]
  (let [lines (u/read-file (str "./data/2021/day16/" file ".txt"))]
    (to-binary-str (first lines))))

(defn print-packet
  ([packet]
   (print-packet packet 0))
  ([packet level]
   (println
     (u/indent level)
     "type:" (:type packet)
     "type-id:" (:type-id packet)
     "version:" (:version packet)
     "length-type-id:" (:length-type-id packet)
     "length:" (:length packet)
     "value:" (:value packet)
     "children:" (count (:children packet))
     "next:" (count (:next packet)))
   (map #(print-packet % (inc level)) (:children packet))))

(defn evaluate-packet [packet]
  (case (:type-id packet)
    4 (:value packet)

    0 (reduce + (map evaluate-packet (:children packet)))
    1 (reduce * (map evaluate-packet (:children packet)))
    2 (reduce min (map evaluate-packet (:children packet)))
    3 (reduce max (map evaluate-packet (:children packet)))
    ; the following 2 are flipped because the children are in reverse order
    5 (if (reduce < (map evaluate-packet (:children packet))) 1 0)
    6 (if (reduce > (map evaluate-packet (:children packet))) 1 0)
    7 (if (reduce = (map evaluate-packet (:children packet))) 1 0)))

(defn parse-file [file]
  (parse-packet (parse file)))

(defn part1 [file]
  (add-versions (parse-file file)))

(defn part2 [file]
  (evaluate-packet (parse-file file)))
