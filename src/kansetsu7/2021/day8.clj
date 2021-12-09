(ns kansetsu7.day8
  (:require
    [clojure.java.io :as io]
    [clojure.math.numeric-tower :as math]
    [clojure.set :as cset]))

(def example-data
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
   edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
   fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
   fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
   aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
   fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
   dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
   bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
   egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
   gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day8.txt"))))
  ([string-data]
   (->> (re-seq #"\w+" string-data)
        (partition 14)
        (mapcat #(partition-all 10 %)))))

(defn count-segments
  [coll]
  (map #(-> % seq count) coll))

(def uniq-seg-cnt->num
  {2 1
   3 7
   4 4
   7 8})

(defn ->segments
  [string]
  (set (re-seq #"\w" string)))

(defn count-uniq-output
  [input]
  (->> (take-nth 2 (rest input))
       flatten
       (filter #(-> % ->segments count uniq-seg-cnt->num))
       count))

(defn ->known-num-segs
  [seg-nums]
  (->> (map ->segments seg-nums)
       (map #(vector % (-> % count uniq-seg-cnt->num)))
       (filter last)
       (into {})))

(defn count5-seg->num
  [seg known-num->seg]
  (let [num4 (known-num->seg 4)
        num1 (known-num->seg 1)]
    (cond
      (cset/superset? seg num1) 3
      (cset/superset? seg (cset/difference num4 num1)) 5
      :else 2)))

(defn count6-seg->num
  [seg known-num->seg]
  (let [num7 (known-num->seg 7)
        num4 (known-num->seg 4)]
    (cond
      (not (cset/superset? seg num7)) 6
      (cset/superset? seg num4) 9
      :else 0)))

(defn unknow-seg->num
  [seg known-num-segs]
  (let [known-num->seg #((cset/map-invert known-num-segs) %)]
    (if (= 5 (count seg))
      (count5-seg->num seg known-num->seg)
      (count6-seg->num seg known-num->seg))))

(defn seg->num
  [seg-num known-num-segs]
  (let [seg (->segments seg-num)]
    (if (known-num-segs seg)
      (known-num-segs seg)
      (unknow-seg->num seg known-num-segs))))

(defn seg-num-mapping
  [seg-nums]
  (let [known-num-segs (->known-num-segs seg-nums)]
    (->> seg-nums
         (map #(vector (->segments %) (seg->num % known-num-segs)))
         (into {}))))

(defn four-digits-val
  [digits]
  (->> (vec digits)
       reverse
       (map-indexed vector)
       (map (fn [[p n]] (* n (math/expt 10 p))))
       (apply +)))

(defn cal-output-nums
  [[ten-nums-segs output-segs]]
  (let [->num (seg-num-mapping ten-nums-segs)]
    (->> output-segs
         (map ->segments)
         (map ->num)
         four-digits-val)))

(defn sum-output-nums
  [input]
  (->> (partition 2 input)
       (map cal-output-nums)
       (apply +)))

;; part1: 369
(defn part1
  []
  (count-uniq-output (puzzle-input)))

;; 1031553
(defn part2
  []
  (sum-output-nums (puzzle-input)))

(comment
  ;; example
  (let [input (puzzle-input example-data)]
    ;; part1
    (count-uniq-output input)
    ;; part2
    (sum-output-nums input))

  (part1)
  (part2))
