(ns kansetsu7.day13
  (:require
    [clojure.java.io :as io]
    [clojure.math.numeric-tower :as math]
    [clojure.string :as cs]
    [kansetsu7.util :as util]))

(def example-data
  "6,10
   0,14
   9,10
   0,3
   10,4
   4,11
   6,0
   6,12
   4,1
   0,13
   10,12
   3,4
   3,0
   8,4
   1,10
   2,14
   8,10
   9,0

   fold along y=7
   fold along x=5")

(defn ->fold-oprations
  [string-data]
  (->> (re-seq #"\w\=\d+" string-data)
       (map #(cs/split % #"\="))
       (map (fn [[string integer]] (vector string (Integer/parseInt integer))))))

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day13.txt"))))
  ([string-data]
   [(->> (re-seq #"\d+,\d+" string-data)
         (map util/find-ints)
         set)
    (->fold-oprations string-data)]))

(defn init-grid
  [points]
  (let [[width height] (->> (util/transpose points)
                           (map #(apply max %))
                           (map inc))]
    (prn :grid-size width :x height)
    (util/init-grid width height " ")))

(defn points->grid
  [points]
  (reduce (fn [grid xy] (util/assoc-in-grid grid xy "#"))
          (init-grid points)
          points))

(defn prn-grid
  [grid]
  (->> grid
       (map #(apply str %))
       (mapv #(prn %))))

(defmulti flip
  (fn [_points [axis _at]] axis))

(defmethod flip "x"
  [points [_axis at]]
  (let [w (inc (* 2 at))]
    (map (fn [[x y]]
           [(-> x (- w) inc math/abs) y])
         points)))

(defmethod flip "y"
  [points [_axis at]]
  (let [h (inc (* 2 at))]
    (map (fn [[x y]]
           [x (-> y (- h) inc math/abs)])
         points)))

(defmulti fold
  (fn [_points [axis _at]] axis))

(defmethod fold "y"
  [points [axis at]]
  (let [up-pts  (filter (fn [[_x y]] (< y at)) points)
        low-pts (filter (fn [[_x y]] (> y at)) points)]
    (set (into up-pts (flip low-pts [axis at])))))

(defmethod fold "x"
  [points [axis at]]
  (let [left-pts (filter (fn [[x _y]] (< x at)) points)
        right-pts (filter (fn [[x _y]] (> x at)) points)]
    (set (into left-pts (flip right-pts [axis at])))))

(defn part1
  []
  (let [[points fold-operations] (puzzle-input)]
    (-> (fold points (first fold-operations))
        count)))

(defn part2
  []
  (let [[points fold-operations] (puzzle-input)]
    (->> (reduce (fn [pts fold-opration]
                   (fold pts fold-opration))
                points
                fold-operations)
         points->grid
         prn-grid)))

(comment
  (let [[points fold-operations] (puzzle-input example-data)]
    (-> (fold points ["y" 7])
        (fold ["x" 5])
        points->grid))
  ;; part1
  (part1)
  ;; part2
  (part2))
