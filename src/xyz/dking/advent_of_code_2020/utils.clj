(ns xyz.dking.advent-of-code-2020.utils
  (:require [clojure.java.io :as io]))

(defn lines-from-input-file
  [filename]
  (line-seq (io/reader (io/resource filename))))
