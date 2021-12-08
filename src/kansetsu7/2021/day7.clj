(ns kansetsu7.day7
  (:require
    [clojure.java.io :as io]
    [kansetsu7.util :as util]
    [clojure.math.numeric-tower :as math]))

(def example-data
  "16,1,2,0,4,2,7,1,2,14")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day7.txt"))))
  ([string-data]
   (vec (util/find-ints string-data))))

(defn move->fuel-cost
  [burn-rate move]
  (if (= :constant burn-rate)
    move
    (/ (* move (inc move)) 2)))

(defn fuel-cost
  [burn-rate positions to]
  (->> positions
       (map #(math/abs (- % to)))
       (map #(move->fuel-cost burn-rate %))
       (apply +)))

(defn least-fuel-option
  [burn-rate positions]
  (let [move-options (range (apply min positions) (inc (apply max positions)))]
    (->> move-options
         (map #(hash-map :move-to % :cost (fuel-cost burn-rate positions %)))
         (apply min-key :cost))))

(comment
  ;; example
  (let [positions (puzzle-input example-data)]
    (:cost (least-fuel-option :constant positions)))

  ;; part1: 359648
  (let [positions (puzzle-input)]
    (:cost (least-fuel-option :constant positions)))
  ;; part2: 100727924
  (let [positions (puzzle-input)]
    (:cost (least-fuel-option :linear positions))))
