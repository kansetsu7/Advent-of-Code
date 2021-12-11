(ns kansetsu7.day12
  (:require
    [clojure.java.io :as io]))

(def example-data
  ["start-A start-b A-c A-b b-d A-end b-end"
   "dc-end HN-start start-kj dc-start dc-HN LN-dc HN-end kj-sa kj-HN kj-dc"
   "fs-end he-DX fs-he start-DX pj-DX end-zg zg-sl zg-pj pj-he RW-he fs-DX pj-RW zg-RW start-pj he-WI zg-he pj-fs start-RW"])

(defn puzzle-input
  ([] (puzzle-input (slurp (io/resource "day12.txt"))))
  ([string-data]
   (->> (re-seq #"\w+" string-data)
        (partition 2))))

(defn possible-directions
  [rough-map]
  (->> rough-map
       (map reverse)
       (into rough-map)
       (remove (fn [[start end]] (or (= "end" start) (= "start" end))))
       (group-by (fn [[start _]] start))
       (map (fn [[start directions]] [start (mapv last directions)]))
       (into {})))

(defn reach-end?
  [path]
  (= "end" (last path)))

(defn small-cave?
  [cave]
  (re-find #"[a-z]" cave))

(defn join-next-cave
  [path possible-dirs]
  (if (reach-end? path)
    [path]
    (->> (get possible-dirs (last path))
         (map #(conj path %)))))

(defn small-cave-visit-frequency
  [path]
  (->> (frequencies path)
       (filter (fn [[cave _]] (small-cave? cave)))))

(defn visit-more-than?
  [n visit-feq]
  (some (fn [[_ visits]] (> visits n)) visit-feq))

(defmulti invalid-access?
  (fn [_path small-cave-access-rule] small-cave-access-rule))

(defmethod invalid-access? :all-small-cave-once
  [path _rule]
  (visit-more-than? 1 (small-cave-visit-frequency path)))

(defmethod invalid-access? :once-small-cave-twice-others-once
  [path _rule]
  (let [visit-feq (small-cave-visit-frequency path)]
    (or (visit-more-than? 2 visit-feq)
        (-> (filter (fn [[_ visits]] (= visits 2)) visit-feq)
            count
            (> 1)))))

(defn join-next-possible-cave
  [exploring-paths possible-dirs small-cave-access-rule]
  (->> exploring-paths
       (mapcat #(join-next-cave % possible-dirs))
       (remove #(invalid-access? % small-cave-access-rule))))

(defn count-ended-paths
  [paths]
  (count (filter reach-end? paths)))

(defn count-all-possible-paths
  [possible-dirs small-cave-access-rule]
  (let [ini-paths (join-next-cave ["start"] possible-dirs)]
    (loop [exploring-paths ini-paths
           ended-paths (count-ended-paths exploring-paths)]
      (if (empty? exploring-paths)
        ended-paths
        (let [exploring-paths' (remove reach-end? exploring-paths)]
          (recur (join-next-possible-cave exploring-paths' possible-dirs small-cave-access-rule)
                 (+ ended-paths (count-ended-paths exploring-paths))))))))

(comment
  ;;part1
  (let [possible-dirs (->> (puzzle-input (nth example-data 2))
                           possible-directions)]
    (count-all-possible-paths possible-dirs :all-small-cave-once))

  ;; part1: 4775
  (time (count-all-possible-paths (possible-directions (puzzle-input)) :all-small-cave-once))
  ;; part2: 152480
  (time (count-all-possible-paths (possible-directions (puzzle-input)) :once-small-cave-twice-others-once)))
