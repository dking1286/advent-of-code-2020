(ns xyz.dking.advent-of-code-2020.day4
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [xyz.dking.advent-of-code-2020.utils :refer [with-input-lines]]))

(def test-lines '("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                  "byr:1937 iyr:2017 cid:147 hgt:183cm"
                  ""
                  "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                  "hcl:#cfa07d byr:1929"
                  ""
                  "hcl:#ae17e1 iyr:2013"
                  "eyr:2024"
                  "ecl:brn pid:760753108 byr:1931"
                  "hgt:179cm"
                  ""
                  "hcl:#cfa07d eyr:2025 pid:166559648"
                  "iyr:2011 ecl:brn hgt:59in"))

(def required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn blank-line?
  [line]
  (= line ""))

(defn non-blank-partition?
  [partition]
  (not= partition [""]))

(defn parse-passport-entry
  [entry]
  (let [[key val] (string/split entry #":")]
    [(keyword key) val]))

(defn parse-passport-line
  [line]
  (into {} (map parse-passport-entry) (string/split line #" ")))

(defn parse-passport
  [lines]
  (transduce (map parse-passport-line) merge lines))

(defn valid?
  [required-fields passport]
  (set/subset? required-fields (set (keys passport))))

(defn part-1
  []
  (with-input-lines [lines "day4_input.txt"]
    (let [xform (comp
                 (partition-by blank-line?)
                 (filter non-blank-partition?)
                 (map parse-passport)
                 (filter (partial valid? required-fields))
                 (map (constantly 1)))]
      (transduce xform + 0 lines))))

(comment
  (let [xform (comp
               (partition-by blank-line?)
               (filter non-blank-partition?)
               (map parse-passport)
               (filter (partial valid? required-fields))
               (map (constantly 1)))]
    (transduce xform + 0 test-lines))
  (part-1))

(def test-lines-2
  (string/split-lines "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

(defn between-inclusive?
  [num lower upper]
  (and (>= num lower)
       (<= num upper)))

(defmulti field-valid? (fn [field _] field))

(defmethod field-valid? :byr
  [_ value]
  (try
    (let [num (Integer/parseInt value)]
      (between-inclusive? num 1920 2002))
    (catch NumberFormatException _
      false)))

(defmethod field-valid? :iyr
  [_ value]
  (try
    (let [num (Integer/parseInt value)]
      (between-inclusive? num 2010 2020))
    (catch NumberFormatException _
      false)))

(defmethod field-valid? :eyr
  [_ value]
  (try
    (let [num (Integer/parseInt value)]
      (between-inclusive? num 2020 2030))
    (catch NumberFormatException _
      false)))

(defmethod field-valid? :hgt
  [_ value]
  (let [[_ num-str unit] (re-matches #"^(\d+)(in|cm)$" value)]
    (try
      (let [num (Integer/parseInt num-str)]
        (case unit
          "cm" (between-inclusive? num 150 193)
          "in" (between-inclusive? num 59 76)
          false))
      (catch NumberFormatException _
        false))))

(defmethod field-valid? :hcl
  [_ value]
  (re-matches #"^#[a-f0-9]{6}$" value))

(comment
  (field-valid? :hcl "aaaaaa"))

(def ^:private eye-colors
  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defmethod field-valid? :ecl
  [_ value]
  (contains? eye-colors value))

(defmethod field-valid? :pid
  [_ value]
  (re-matches #"^\d{9}$" value))

(defmethod field-valid? :default
  [_ _]
  false)

(defn really-valid?
  [required-fields passport]
  (->> required-fields
       (every? (fn [field]
                 (and (contains? passport field)
                      (field-valid? field (field passport)))))))

(defn part-2
  []
  (with-input-lines [lines "day4_input.txt"]
    (let [xform (comp
                 (partition-by blank-line?)
                 (filter non-blank-partition?)
                 (map parse-passport)
                 (filter (partial really-valid? required-fields))
                 (map (constantly 1)))]
      (transduce xform + 0 lines))))

(comment
  (Integer/parseInt "#333")
  (let [xform (comp
               (partition-by blank-line?)
               (filter non-blank-partition?)
               (map parse-passport)
              ;;  (map (fn [passport]
              ;;         (into {}
              ;;               (map (fn [k]
              ;;                      (let [v (k passport)]
              ;;                        [k (boolean (field-valid? k v))])))
              ;;               required-fields)))
               (filter (partial really-valid? required-fields))
              ;;  (map (constantly 1))
               )]
    (transduce xform conj [] test-lines-2))
  (part-2))

;; 99 fails