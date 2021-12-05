(ns kansetsu7.day3
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [clojure.math.numeric-tower :as math]))

(def example-draw-list
  [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1])

(def draw-list
  (let [str-list "42,44,71,26,70,92,77,45,6,18,79,54,31,34,64,32,16,55,81,11,90,10,21,87,0,84,8,23,1,12,60,20,57,68,61,82,49,59,22,2,63,33,50,39,28,30,88,41,69,72,98,73,7,65,53,35,96,67,36,4,51,75,24,86,97,85,66,29,74,40,93,58,9,62,95,91,80,99,14,19,43,37,27,56,94,25,83,48,17,38,78,15,52,76,5,13,46,89,47,3"]
      (->> (cs/split str-list #",")
           (map #(Integer/parseInt %)))))

(def exaple-data
  "22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7")

(defn str->int-seq
  [string ptrn]
  (->> (cs/split (cs/trim string) ptrn)
       (remove empty?)
       (map #(Integer/parseInt %))))

(defn str-coll->board
  [coll]
  (map #(str->int-seq % #" ") coll))

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day4.txt"))))
  ([string-data]
   (->> (-> string-data
            (cs/split #"\n\n"))
       (map #(cs/split % #"\n"))
       (map str-coll->board))))

(defn bingo?
  [col-or-row drawn-nums]
  (every? (set drawn-nums) col-or-row))

(defn bingo-row
  [board drawn-nums]
  (filter #(bingo? % drawn-nums) board))

(defn transpose
  [board]
  (apply mapv vector board))

(defn bingo-col
  [board drawn-nums]
  (filter #(bingo? % drawn-nums) (transpose board)))

(defn bingo-nums
  [board drawn-nums]
  (or (seq (bingo-row board drawn-nums))
      (seq (bingo-col board drawn-nums))))

(defn marked?
  [number drawn-nums]
  ((set drawn-nums) number))

(defn unmarked-sum
  [board drawn-nums]
  (->> (flatten board)
       (remove #(marked? % drawn-nums))
       (apply +)))

(defn cal-score
  [board drawn-nums]
  (* (unmarked-sum board drawn-nums)
     (last drawn-nums)))

(defn board-info
  [board draw-list]
  (loop [nth-draw 1]
    (let [drawn-nums (take nth-draw draw-list)]
      (cond
        (> nth-draw (count draw-list)) {:win? false}
        (bingo-nums board drawn-nums) {:win? true :nth-draw nth-draw :score (cal-score board drawn-nums)}
        :else (recur (inc nth-draw))))))

(defn get-win-board
  [boards draw-list]
  (->> (map #(board-info % draw-list) boards)
       (filter #(:win? %))
       (sort-by :nth-draw)
       first))

(comment
  ;; example
  (let [boards (puzzle-input exaple-data)]
    (:score (get-win-board boards example-draw-list)))
  ;; part1
  (get-win-board (puzzle-input) draw-list))
