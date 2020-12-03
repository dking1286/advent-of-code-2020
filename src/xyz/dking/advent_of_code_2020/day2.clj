(ns xyz.dking.advent-of-code-2020.day2
  (:require [xyz.dking.advent-of-code-2020.utils :as utils]))

(def line-regexp #"(\d+)-(\d+) (\w): (\w+)")

(defn parse-line
  [line]
  (let [[_ min-str max-str letter password] (re-matches line-regexp line)]
    {:lower-bound (Integer/parseInt min-str)
     :upper-bound (Integer/parseInt max-str)
     :letter (first letter)
     :password password}))

(defn num-occurances
  [char string]
  (count (filter #(= char %) string)))

(defn valid?
  [{:keys [lower-bound upper-bound letter password]}]
  (let [occurances (num-occurances letter password)]
    (and (>= occurances lower-bound)
         (<= occurances upper-bound))))

(defn part-1
  []
  (->> (utils/lines-from-input-file "day2_input.txt")
       (map parse-line)
       (filter valid?)
       count))

(comment
  (parse-line "5-99 g: ggccggmgn")
  (num-occurances \c "ccac")
  (valid? {:lower-bound 1 :upper-bound 2 :letter \a :password "abcdeaa"})
  (part-1))

(defn really-valid?
  [{:keys [lower-bound upper-bound letter password]}]
  (let [lower-char (.charAt password (dec lower-bound))
        upper-char (.charAt password (dec upper-bound))]
    (or (and (= lower-char letter) (not= upper-char letter))
        (and (= upper-char letter) (not= lower-char letter)))))

(defn part-2
  []
  (->> (utils/lines-from-input-file "day2_input.txt")
       (map parse-line)
       (filter really-valid?)
       count))

(comment
  (really-valid? {:lower-bound 1 :upper-bound 2 :letter \a :password "abcdeaa"})
  (part-2))