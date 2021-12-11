(ns kansetsu7.day11
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [kansetsu7.util :as util]))

(def example-data
  "11111
   19991
   19191
   19991
   11111")

(def example-data-2
  "5483143223
   2745854711
   5264556173
   6141336146
   6357385478
   4167524645
   2176841721
   6882881134
   4846848554
   5283751526")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day11.txt"))))
  ([string-data]
   (->> (cs/split string-data #"\n")
        (map util/find-digits)
        (mapv vec))))

(defn inc-every
  [octopuses]
  (mapv #(mapv inc %) octopuses))

(defn filter-lv
  [f octopuses]
  (->> (flatten octopuses)
       (map-indexed vector)
       (filter (fn [[_ lv]] (f lv)))
       (map (fn [[idx _]] idx))))

(defn flash-indexes
  [octopuses]
  (filter-lv #(>= % 10) octopuses))

(defn inc-adjacents
  [octopuses idx]
  (let [width (-> octopuses first count)
        height (count octopuses)
        [x y] (util/idx->xy idx width)
        adjacents (util/find-adjacent-coordinates [x y] [width height])]
    (reduce #(util/update-matrix %1 %2 inc) octopuses adjacents)))

(defn flash-off
  [octopuses indexes]
  (let [width (-> octopuses first count)
        turn-off #(- % ##Inf)]
    (reduce #(util/update-matrix %1 (util/idx->xy %2 width) turn-off) octopuses indexes)))

(defn every-off->0
  [octopuses]
  (let [off->0 #(if (= ##-Inf %) 0 %)]
    (mapv #(mapv off->0 %) octopuses)))

(defn add-step
  [octopuses]
  (let [octopuses' (inc-every octopuses)]
    (loop [oct octopuses']
      (let [flashes (flash-indexes oct)]
        (if (empty? flashes)
          (every-off->0 oct)
          (recur (-> (reduce #(inc-adjacents %1 %2) oct flashes)
                     (flash-off flashes))))))))

(defn flash-count
  [octopuses]
  (count (filter-lv zero? octopuses)))

(defn after-n-steps
  [n octopuses]
  (-> (iterate (fn [{:keys [octopuses flashes]}]
                 (let [oct' (add-step octopuses)]
                   {:octopuses oct' :flashes (+ flashes (flash-count oct'))}))
               {:octopuses octopuses :flashes 0})
      (nth n)))

(defn synchronized?
  [octopuses]
  (->> (flatten octopuses)
       (every? zero?)))

(defn synchronized-step
  [octopuses]
  (loop [n 0
         oct octopuses]
    (if (synchronized? oct)
      n
      (recur (inc n)
             (add-step oct)))))


(comment
  ;;part1
  (:flashes (after-n-steps 10 (puzzle-input example-data-2)))  ;; 204
  (:flashes (after-n-steps 100 (puzzle-input example-data-2))) ;; 1656
  ;; part2
  (synchronized-step (puzzle-input example-data-2)) ;; 195

  ;; part1: 1739
  (:flashes (after-n-steps 100 (puzzle-input)))
  ;; part2: 324
  (synchronized-step (puzzle-input)))
