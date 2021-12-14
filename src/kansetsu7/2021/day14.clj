(ns kansetsu7.day14
  (:require
    [clojure.java.io :as io]))

(def example-data
  "NNCB

   CH -> B
   HH -> N
   CB -> H
   NH -> C
   HB -> C
   HC -> B
   HN -> C
   NN -> C
   BH -> H
   NC -> B
   NB -> B
   BN -> B
   BB -> N
   BC -> B
   CC -> N
   CN -> C")

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day14.txt"))))
  ([string-data]
   (let [data (re-seq #"\w+" string-data)]
     [(->> data first seq)
      (->> (rest data)
           (map seq)
           (partition 2)
           (map (fn [[pair insertion]] [pair (first insertion)]))
           (into {}))])))

(defn multiply-all-vals
  [n m]
  (reduce (fn [memo k] (update memo k * n)) m (keys m)))

(defn reaction
  [pair feq rules]
  (let [insertion (get rules pair)]
    (->> (vector (first pair) insertion (last pair))
         (partition 2 1)
         frequencies
         (multiply-all-vals feq))))

(defn chain-reaction
  [pair-feq rules]
  (reduce (fn [memo [pair feq]]
            (merge-with + memo (reaction pair feq rules)))
          {}
          pair-feq))

(defn max-min-diff
  [pair-feq last-element]
  (let [counts (->> pair-feq
                    (map (fn [[[e1 _] feq]] (hash-map e1 feq)))
                    (into [{last-element 1}])
                    (apply merge-with +)
                    vals)]
    (- (apply max counts)
       (apply min counts))))

(defn diff-after-steps
  ([steps input]
   (let [[template rules] input
         pair-feq (->> template (partition 2 1) frequencies)]
     (-> (iterate #(chain-reaction % rules) pair-feq)
         (nth steps)
         (max-min-diff (last template))))))

(comment
  (diff-after-steps 10 (puzzle-input example-data))
  ;; part1: 2027
  (diff-after-steps 10 (puzzle-input))
  ;; part2: 2265039461737
  (diff-after-steps 40 (puzzle-input)))











