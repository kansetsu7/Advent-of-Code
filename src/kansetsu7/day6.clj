(ns kansetsu7.day6
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(def example-data
  "3,4,3,1,2")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day6.txt"))))
  ([string-data]
   (->> (cs/split string-data #",")
        (map cs/trim)
        (map #(Integer/parseInt %))
        vec)))

(defn update-timer
  [timer]
  (if (zero? timer) 6 (dec timer)))

(defn update-timer-map-keys
  [m]
  (->> m
      (map (fn [[k v]] (hash-map (update-timer k) v)))
      (apply merge-with +)))

(defn next-day
  [timer-map]
  (let [new-fishes (or (get timer-map 0) 0)]
    (cond-> (update-timer-map-keys timer-map)
      (> new-fishes 0) (update 8 #(+ (or % 0) new-fishes)))))

(defn days-pass
  [timers days]
  (loop [timer-map   (frequencies timers)
         passed-days 0]
    (if (>= passed-days days)
      (apply + (vals timer-map))
      (recur (next-day timer-map)
             (inc passed-days)))))

(defn part1
  []
  (days-pass (puzzle-input) 80)) ;; 350605

(defn part2
  []
  (days-pass (puzzle-input) 256)) ;; 1592778185024

(comment
  ;; example
  (let [ini-fishes (puzzle-input example-data)]
    (days-pass ini-fishes 18)
    (days-pass ini-fishes 80))

  (part1)
  (part2))
