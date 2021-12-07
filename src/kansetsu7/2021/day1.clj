(ns kansetsu7.day1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(defn puzzle-input
  []
  (as-> (slurp (io/resource "day1.txt")) $
        (cs/split $ #"\n")
        (map #(Integer/parseInt %) $)))

(defn increase?
  [[previous curent]]
  (< previous curent))

(defn get-increase
  [coll]
  (->> coll
      (partition 2 1)
      (filter increase?)
      count))

(comment
  ;; part1: 1692
  (-> (puzzle-input)
      get-increase)

  ;; part2: 1724
  (->> (partition 3 1 (puzzle-input))
       (map #(apply + %))
       get-increase))

