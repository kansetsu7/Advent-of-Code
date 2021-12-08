(ns kansetsu7.day8
  (:require
    [clojure.java.io :as io]))

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

(def uniq-num-segs->num
  {2 1
   3 7
   4 4
   7 8})

(defn extract-output
  [input]
  (->> (take-nth 2 (rest input))
       flatten))

(defn count-uniq-output
  [input]
  (->> (extract-output input)
       count-segments
       (filter #(uniq-num-segs->num %))
       count))

(comment
  ;; example
  (let [input (puzzle-input example-data)]
    (count-uniq-output input))

  ;; part1: 369
  (let [input (puzzle-input)]
    (count-uniq-output input)))