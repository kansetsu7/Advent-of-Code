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

(defn map-nth
  [coll position]
  (map #(nth % position) coll))

(defn get-most-common
  [nums]
  (let [feq (frequencies nums)]
    (if (>= (get feq 1) (get feq 0)) 1 0)))

(defn get-most-common-at
  [nums position]
  (->> (map-nth nums position)
       get-most-common))

(defn revert-bit
  [bit]
  (if (zero? bit) 1 0))

(defn revert-bits
  [binary]
  (map revert-bit binary))

(defn bit-power->decimal
  [[power bit]]
  (* bit (math/expt 2 power)))

(defn binary->deciaml
  [binary]
  (->> (reverse binary)
       (map-indexed vector)
       (reduce #(+ %1 (bit-power->decimal %2)) 0)))

(defn filter-at-position
  [binary-coll rate-type position]
  (let [criteria (cond-> (get-most-common-at binary-coll position)
                   (= rate-type :CO2) revert-bit)]
    (filter #(= criteria (nth % position)) binary-coll)))

(defn get-rating
  [input rate-type]
  (loop [binary-coll input
         position    0]
    (if (= 1 (count binary-coll))
      (-> binary-coll first binary->deciaml)
      (recur (filter-at-position binary-coll rate-type position)
             (inc position)))))

(comment
  ;; part1: 3959450
  (let [input (puzzle-input)
        bits (-> input first count)
        gamma-binary (map #(get-most-common-at input %) (range bits))
        epsilon-binary (revert-bits gamma-binary)]
    (* (binary->deciaml gamma-binary)
       (binary->deciaml epsilon-binary)))

  ;; part2: 7440311
  (let [input (puzzle-input)]
    (* (get-rating input :CO2)
       (get-rating input :O2))))

  ;; for debug
  ;; "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
  ;; (defn puzzle-input-2
  ;;   [input]
  ;;   (->> (cs/split input #"\n")
  ;;        (map #(-> % seq chars->int)))))

