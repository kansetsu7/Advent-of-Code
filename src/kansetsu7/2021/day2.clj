(ns kansetsu7.day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(defn puzzle-input
  []
  (->> (cs/split (slurp (io/resource "day2.txt")) #"\n")
       (map #(cs/split % #" "))
       (map (fn [[dir X]] (vector dir (Integer/parseInt X))))))

(defn sum-hr
  [course]
  (->> course
       (map (fn [[dir X]] (if (= "forward" dir) X 0)))
       (apply +)))

(defn sum-depth
  [course]
  (->> course
       (map (fn [[dir X]] (case dir "up" (* -1 X) "down" X 0)))
       (apply +)))

(defn sum-hr-depth
  [[hr depth] [dir X]]
  (case dir
    "up"      [hr (- depth X)]
    "down"    [hr (+ depth X)]
    "forward" [(+ hr X) depth]))

(defn get-destination
  [[hr depth aim] [dir X]]
  (case dir
    "up"      [hr depth (- aim X)]
    "down"    [hr depth (+ aim X)]
    "forward" [(+ hr X) (+ depth (* aim X)) aim]))

(defn multiply-hr-depth
  [[hr depth _aim]]
  (* hr depth))

(comment
  ;; part1: 1250395
  (let [input (puzzle-input)]
    (* (sum-hr input) (sum-depth input)))

  ;; another approach of part1
  (let [input (puzzle-input)]
    (->> (reduce sum-hr-depth [0 0] input)
         (apply *)))

  ;; part2: 1451210346
  (->> (puzzle-input)
       (reduce get-destination [0 0 0])
       multiply-hr-depth))
