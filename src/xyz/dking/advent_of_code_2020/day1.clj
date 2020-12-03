(ns xyz.dking.advent-of-code-2020.day1
  (:require [clojure.java.io :as io]))

(defn multiply-pair-with-sum
  [sum nums]
  (when-let [num (first (filter #(contains? nums (- sum %)) nums))]
    (* num (- sum num))))

(defn part-1
  []
  (->> (line-seq (io/reader (io/resource "day1_input.txt")))
       (map #(Integer/parseInt %))
       (into #{})
       (multiply-pair-with-sum 2020)))

(comment
  (multiply-pair-with-sum 1000 #{1 2 3 4})
  (part-1))

(defn distinct-triples
  [nums]
  (for [x nums
        y nums :when (not= y x)
        z nums :when (and (not= z x) (not= z y))]
    [x y z]))

(defn multiply-triple-with-sum
  [sum nums]
  (->> (distinct-triples nums)
       (filter (fn [[x y z]] (= sum (+ x y z))))
       first
       (apply *)))

(defn part-2
  []
  (->> (line-seq (io/reader (io/resource "day1_input.txt")))
       (map #(Integer/parseInt %))
       (multiply-triple-with-sum 2020)))

(comment
  (distinct-triples [1 2 3 4])
  (multiply-triple-with-sum 6 [1 2 3 4])
  (part-2))