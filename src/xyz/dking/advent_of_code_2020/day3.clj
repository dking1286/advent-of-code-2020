(ns xyz.dking.advent-of-code-2020.day3
  (:require [xyz.dking.advent-of-code-2020.utils :refer [lines-from-input-file
                                                         with-input-lines]]))

(def test-lines '("..##......."
                  "#...#...#.."
                  ".#....#..#."
                  "..#.#...#.#"
                  ".#...##..#."
                  "..#.##....."
                  ".#.#.#....#"
                  ".#........#"
                  "#.##...#..."
                  "#...##....#"
                  ".#..#...#.#"))

(defn tree?
  [line x-coord]
  (= \# (.charAt line x-coord)))

(defn count-trees
  [lines x-slope y-slope]
  (loop [remaining-lines lines
         x-coord 0
         trees-count 0]
    (let [line (first remaining-lines)]
      (if-not line
        trees-count
        (recur (drop y-slope remaining-lines)
               (mod (+ x-coord x-slope) (count line))
               (if (tree? line x-coord)
                 (inc trees-count)
                 trees-count))))))

(defn part-1
  []
  (with-input-lines [lines "day3_input.txt"]
    (count-trees lines 3 1)))

(comment
  (count-trees test-lines 3 1)
  (part-1))

(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(defn part-2
  []
  (let [lines (lines-from-input-file "day3_input.txt")]
    (->> slopes
         (map (fn [[x-slope y-slope]] (count-trees lines x-slope y-slope)))
         (apply *))))

(comment
  (let [lines test-lines]
    (->> slopes
         (map (fn [[x-slope y-slope]] (count-trees lines x-slope y-slope)))))
  (part-2))

