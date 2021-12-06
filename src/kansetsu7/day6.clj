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

(defn new-fish-counts
  [timers]
  (-> (frequencies timers)
      (get 0)))

(defn create-new-fishes
  [timers]
  (if-let [cnt (new-fish-counts timers)]
    (repeat cnt 8)
    []))


(defn next-day
  [timers]
  (->> (map update-timer timers)
       (apply conj (create-new-fishes timers))))

(defn days-pass
  [timers days]
  (loop [timers timers
         passed-days 0]
    (if (>= passed-days days)
      timers
      (recur (next-day timers)
             (inc passed-days)))))

(comment
  ;; example
  (let [ini-fishes (puzzle-input example-data)]
    ;; part1
    (count (days-pass ini-fishes 18))
    (count (days-pass ini-fishes 80)))
  ;; part1
  (let [ini-fishes (puzzle-input)]
    ;; part1
    (count (days-pass ini-fishes 80))))


