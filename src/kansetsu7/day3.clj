(ns kansetsu7.day3
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [clojure.math.numeric-tower :as math]))

(defn chars->int
  [char-coll]
  (map #(-> % str Integer/parseInt) char-coll))

(defn puzzle-input
  []
  (->> (cs/split (slurp (io/resource "day3.txt")) #"\n")
       (map #(-> % seq chars->int))))

(defn count-zero
  [zero-counts binaries]
  (map #(cond-> %1 (zero? %2) inc) zero-counts binaries))

(defn most-common-num
  [lines zero-counts]
  (if (> zero-counts (/ lines 2)) 0 1))

(defn ->decimal
  [[power bi-digit]]
  (* bi-digit (math/expt 2 power)))

(defn binary->deciaml
  [binary]
  (->> (reverse binary)
       (map-indexed vector)
       (reduce #(+ %1 (->decimal %2)) 0)))

(comment
  ;; part1: 3959450
  (let [input (puzzle-input)
        lines (count input)
        gamma-binary (->> input
                          (reduce count-zero (repeat 0))
                          (map #(most-common-num lines %)))
        epsilon-binary (map #(if (zero? %) 1 0) gamma-binary)]
    (* (binary->deciaml gamma-binary)
       (binary->deciaml epsilon-binary))))
