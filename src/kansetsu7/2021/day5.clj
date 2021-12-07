(ns kansetsu7.day5
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(def example-data
  "0,9 -> 5,9
   8,0 -> 0,8
   9,4 -> 3,4
   2,2 -> 2,1
   7,0 -> 7,4
   6,4 -> 2,0
   0,9 -> 2,9
   3,4 -> 1,4
   0,0 -> 8,8
   5,5 -> 8,2")

(defn coor-str->coor-int
  [coor-str]
  (map #(Integer/parseInt %) coor-str))

(defn line-str->line-coor
  [line-str]
  (->> (cs/split line-str #" -> ")
       (map #(cs/split % #","))
       (map coor-str->coor-int)))

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day5.txt"))))
  ([string-data]
   (->> (cs/split string-data #"\n")
        (map cs/trim)
        (map line-str->line-coor))))

(defn get-max-x
  [[p1 p2]]
  (max (first p1) (first p2)))

(defn get-max-y
  [[p1 p2]]
  (max (last p1) (last p2)))

(defn get-heatmap-size
  [lines]
  [(apply max (map get-max-x lines))
   (apply max (map get-max-y lines))])

(defn init-heatmap
  [[x-size y-size]]
  (let [row (vec (repeat (inc x-size) 0))]
    (vec (repeat (inc y-size) row))))

(defn horizontal?
  [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn vertical?
  [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn hv-line?
  "tell if horizontal or vertical line"
  [line]
  (or (horizontal? line)
      (vertical? line)))

(defn expand-h-points
  [[[x1 y] [x2 _]]]
  (let [start (min x1 x2)
        end   (inc (max x1 x2))]
    (map #(vector % y) (range start end))))

(defn expand-v-points
  [[[x y1] [_ y2]]]
  (let [start (min y1 y2)
        end   (inc (max y1 y2))]
    (map #(vector x %) (range start end))))

(defn expand-diagonal-points
  [[[x1 y1] [x2 y2]]]
  (let [start-x (min x1 x2)
        end-x   (inc (max x1 x2))
        start-y (min y1 y2)
        end-y   (inc (max y1 y2))
        x-range (cond-> (range start-x end-x)
                  (> x1 x2) reverse)
        y-range (cond-> (range start-y end-y)
                  (> y1 y2) reverse)]
    (map #(vector %1 %2) x-range y-range)))

(defn cover-points
  [line]
  (cond
    (horizontal? line) (expand-h-points line)
    (vertical? line) (expand-v-points line)
    :else (expand-diagonal-points line)))

(defn add-point-to-heatmap
  [heatmap point]
  (update-in heatmap (reverse point) inc))

(defn update-heatmap
  [heatmap points]
  (reduce #(add-point-to-heatmap %1 %2) heatmap points))

(defn count-safe-points
  [vent-lines {:keys [exclude-diagonal]}]
  (let [ini-heatmap (-> vent-lines get-heatmap-size init-heatmap)]
    (->> vent-lines
         (filter #(cond-> % exclude-diagonal hv-line?))
         (mapcat cover-points)
         (update-heatmap ini-heatmap)
         flatten
         (filter #(>= % 2))
         count)))

(comment
  ;; example
  (let [vent-lines (puzzle-input example-data)]
    ;; part1
    (count-safe-points vent-lines {:exclude-diagonal true})
    ;; part2
    (count-safe-points vent-lines {:exclude-diagonal false}))

  ;; part1: 5124
  (let [vent-lines (puzzle-input)]
    (count-safe-points vent-lines {:exclude-diagonal true}))
  ;; part2: 19771
  (let [vent-lines (puzzle-input)]
    (count-safe-points vent-lines {:exclude-diagonal false})))
