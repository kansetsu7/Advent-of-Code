(ns kansetsu7.util
  (:require
    [clojure.string :as cs]))

;; general
(defn blank?
  [v]
  (if (seqable? v) (empty? v) (not v)))

(def present?  (complement blank?))

;; strings
(defn find-ints
  [str-lines]
  (->> (re-seq #"\d+" str-lines)
       (map #(Integer/parseInt %))))

(defn find-digits
  [str-lines]
  (->> (re-seq #"\d" str-lines)
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

;; grid
(defn init-grid
  ([w h] (init-grid w h nil))
  ([w h ini-val]
   (vec (repeat h (vec (repeat w ini-val))))))

(defn update-in-grid
  "Update a vector of vector grid, index start from 0"
  [grid [x y] f]
  (update-in grid [y x] f))

(defn find-adjacent-coordinates
  ([[x y] [width height]] (find-adjacent-coordinates [x y] [width height] {:include-diagonally? true}))
  ([[x y] [width height] {:keys [include-diagonally?]}]
   (let [valid-x? #(and (>= % 0) (<= % (dec width)))
         valid-y? #(and (>= % 0) (<= % (dec height)))
         coordinates (cond-> [[(inc x) y]
                              [(dec x) y]
                              [x (inc y)]
                              [x (dec y)]]
                       include-diagonally?
                       (into [[(inc x) (inc y)]
                              [(inc x) (dec y)]
                              [(dec x) (inc y)]
                              [(dec x) (dec y)]]))]
     (filter (fn [[ix iy]] (and (valid-x? ix) (valid-y? iy))) coordinates))))

(defn idx->xy
  [idx width]
  [(mod idx width)
   (-> idx (/ width) int)])
