(ns kansetsu7.day7
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [clojure.math.numeric-tower :as math]))

(def example-data
  "16,1,2,0,4,2,7,1,2,14")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day7.txt"))))
  ([string-data]
   (->> (cs/split string-data #",")
        (map cs/trim)
        (map #(Integer/parseInt %))
        vec)))

(defn fuel-cost
  [positions to]
  (->> positions
       (map #(math/abs (- % to)))
       (apply +)))

(defn least-fuel-option
  [positions]
  (let [move-options (range (apply min positions) (inc (apply max positions)))]
    (->> move-options
         (map #(hash-map :move-to % :cost (fuel-cost positions %)))
         (apply min-key :cost))))

(comment
  ;; example
  (let [positions (puzzle-input example-data)]
    (:cost (least-fuel-option positions)))

  ;; part1
  (let [positions (puzzle-input)]
    (:cost (least-fuel-option positions))))
