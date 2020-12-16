(ns adventofcode.2020.day4)
(require '[adventofcode.util :as u]
         '[clojure.string :as str])

(defn parse-passport-line
  [line]
  (let [tokens (str/split line #" ")
        fields (map #(str/split % #":") tokens)]
    (u/pairs-to-map fields)))

(defn parse-input
  [lines passports passport]
  (let [first (first lines)
        rest (rest lines)]
    (cond
      (empty? lines) (cons passport passports)
      (empty? first) (parse-input rest (cons passport passports) {})
      :else (let [new-fields (parse-passport-line first)]
              (parse-input rest passports (conj passport new-fields))))))

(defn check-valid-passport
  [validations passport]
  (let [first-validation (first validations) rest (rest validations)]
    (if (empty? validations)
      true
      (and
        (let [key (first first-validation) validator-function (second first-validation)]
          (validator-function (passport key)))
        (check-valid-passport rest passport)))))

(defn hgt [hgt]
  (let [matches (re-matches #"(\d+)(cm|in)" hgt)]
    (cond
      (nil? matches) false
      :else (let [hgt-num (u/to-int (second matches))]
              (if (= (last matches) "cm")
                (and (>= hgt-num 150) (<= hgt-num 193))
                (and (>= hgt-num 59) (<= hgt-num 76)))))))

(defn solution []
  (let [data (u/read-file "./data/day/4/input.txt")
        passports (parse-input data [] {})
        validations-1 (u/into-map #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} #(not (nil? %)))
        validations-2 {"byr" (u/safe-predicate #(u/range-match? % 1920 2002))
                       "iyr" (u/safe-predicate #(u/range-match? % 2002 2020))
                       "eyr" (u/safe-predicate #(u/range-match? % 2020 2030))
                       "hgt" (u/safe-predicate hgt)
                       "hcl" (u/safe-predicate #(u/regex-match? #"^#[0-9a-f]{6}$" %))
                       "ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
                       "pid" (u/safe-predicate #(u/regex-match? #"^[0-9]{9}$" %))}]
    {:part1 (count (filter #(check-valid-passport validations-1 %) passports))
     :part2 (count (filter #(check-valid-passport validations-2 %) passports))}))
