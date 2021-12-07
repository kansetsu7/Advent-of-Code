(ns kansetsu7.util
  (:require
    [clojure.string :as cs]))

;; strings
(defn find-ints
  [str-lines]
  (->> (re-seq #"\d+" str-lines)
       (map #(Integer/parseInt %))))

(defn ->str-int-pairs
  [str-lines]
  (->> (cs/split str-lines #"\n")
       (map #(cs/split % #" "))
       (map (fn [[string integer]] (vector string (Integer/parseInt integer))))))

(defn chars->int
  [char-coll]
  (map #(-> % str Integer/parseInt) char-coll))

;; vector
(defn transpose
  [board]
  (apply mapv vector board))
