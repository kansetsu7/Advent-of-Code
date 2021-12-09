(ns kansetsu7.day10
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cs]))

(def example-data
  "[({(<(())[]>[[{[]{<()<>>
   [(()[<>])]({[<{<<[]>>(
   {([(<{}[<>[]}>{[]{[(<()>
   (((({<>}<{<{<>}{[]{[]{}
   [[<[([]))<([[{}[[()]]]
   [{[{({}]{}}([{[{{{}}([]
   {<[[]]>}<{[{[{[]{()[[[]
   [<(<(<(<{}))><([]([]()
   <{([([[(<>()){}]>(<<{{
   <{([{{}}[<[[[<>{}]]]>[]]")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day10.txt"))))
  ([string-data]
   (->> (cs/split string-data #"\n")
        (map cs/trim))))

(def legal-chunk-ptrn
  #"(\[\]|\(\)|<>|\{\})")

(def left-ptrn
  #"(\[|\(|<|\{)")

(def right-ptrn
  #"(\]|\)|>|\})")

(defn remove-legal-chunk
  [line]
  (loop [s line]
    (if (re-find legal-chunk-ptrn s)
      (recur (cs/replace s legal-chunk-ptrn ""))
      s)))

(defn all-same-side?
  [line]
  (not (and (re-seq left-ptrn line)
            (re-seq right-ptrn line))))

(defn keep-illegal-lines
  [lines]
  (->> (map remove-legal-chunk lines)
       (remove all-same-side?)))

(defn get-first-illegal
  [line]
  (->> (seq line)
       (partition 2 1)
       (map #(apply str %))
       (remove all-same-side?)
       first
       (re-find #".$")))

(defn char->pt
  [c]
  ({")" 3
    "]" 57
    "}" 1197
    ">" 25137} c))

(defn syntax-error-score
  [lines]
  (->> (keep-illegal-lines lines)
       (map get-first-illegal)
       (map char->pt)
       (apply +)))

(comment
  ;; example
  (let [input (puzzle-input example-data)]
    (syntax-error-score input))


  ;; part1
  (syntax-error-score (puzzle-input)))

  ;; part2
