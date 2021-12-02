(ns kansetsu7.day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(defn puzzle-input
  []
  (->> (cs/split (slurp (io/resource "day2.txt")) #"\n")
       (map #(cs/split % #" "))
       (map (fn [[direction amt]] (vector direction (Integer/parseInt amt))))))

(defn sum-x
  [coll]
  (->> (map (fn [[dir amt]] (if (= "forward" dir) amt 0)) coll)
       (apply +)))

(defn sum-y
  [coll]
  (->> (map (fn [[dir amt]] (case dir "up" (* -1 amt) "down" amt 0)) coll)
       (apply +)))

(comment
  ;; part1: 1250395
  (let [input (puzzle-input)]
    (* (sum-x input) (sum-y input))))
