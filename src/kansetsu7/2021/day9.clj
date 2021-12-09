(ns kansetsu7.day9
  (:require
    [clojure.java.io :as io]
    [kansetsu7.util :as util]))

(def example-data
  "2199943210
   3987894921
   9856789892
   8767896789
   9899965678")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day9.txt"))))
  ([string-data]
   (->> (re-seq #"\d+" string-data)
        (map #(re-seq #"\d" %))
        (map #(map (fn [n] (Integer/parseInt n)) %)))))

(defn add-infinity-to-both-sides
  [l]
  (->> l
       (cons ##Inf)
       reverse
       (cons ##Inf)
       reverse))

(defmulti adjacents
  (fn [adj-type _heightmap] adj-type))

(defmethod adjacents :row
  [_ heightmap]
  (->> heightmap
       (map add-infinity-to-both-sides)
       (mapcat #(partition 3 1 %))))

(defmethod adjacents :col
  [_ heightmap]
  (->> heightmap
       util/transpose
       (map add-infinity-to-both-sides)
       (map #(partition 3 1 %))
       util/transpose
       (mapcat identity)))

(defn combinations
  [heightmap]
  (util/transpose [(adjacents :row heightmap)
                   (adjacents :col heightmap)]))

(defn low-point
  [[[l p r] [t _ b]]]
  (if (and (> l p) (> r p) (> t p) (> b p))
    p
    nil))

(defn points->risk-lv
  [points]
  (+ (count points)
     (apply + points)))

(defn risk-lv
  [heightmap]
  (->> (combinations heightmap)
       (map low-point)
       (remove nil?)
       points->risk-lv))

(comment
  (cons 99 '(1 2 3 4))
  ;; example
  (let [input (puzzle-input example-data)]
    (risk-lv input))

  ;; part1
  (risk-lv (puzzle-input)))

