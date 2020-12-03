(ns xyz.dking.advent-of-code-2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn lines-from-input-file
  [filename]
  (s/split-lines (slurp (io/resource filename))))

(defmacro with-input-lines
  [[lines-sym filename] & body]
  `(with-open [rdr# (io/reader (io/resource ~filename))]
     (let [~lines-sym (line-seq rdr#)]
       ~@body)))

(comment
  (lines-from-input-file "day1_input.txt")
  (with-input-lines [lines "day1_input.txt"]
    (doall lines)))