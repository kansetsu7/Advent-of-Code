(ns kansetsu7.day12
  (:require
    [clojure.java.io :as io]))

(def example-data
  "start-A
   start-b
   A-c
   A-b
   b-d
   A-end
   b-end")

(def example-data-2
  "dc-end
   HN-start
   start-kj
   dc-start
   dc-HN
   LN-dc
   HN-end
   kj-sa
   kj-HN
   kj-dc")

(def example-data-3
  "fs-end
   he-DX
   fs-he
   start-DX
   pj-DX
   end-zg
   zg-sl
   zg-pj
   pj-he
   RW-he
   fs-DX
   pj-RW
   zg-RW
   start-pj
   he-WI
   zg-he
   pj-fs
   start-RW")

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
       (map vec)))

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
    (->> (filter #(= (last path) (first %)) possible-dirs)
         (map #(conj path (last %))))))

(defn invalid-access?
  [path]
  (->> (frequencies path)
       (some (fn [[cave visits]] (and (small-cave? cave) (> visits 1))))))

(defn join-nex-possible-cave
  [exploring-paths possible-dirs]
  (->> exploring-paths
       (mapcat #(join-next-cave % possible-dirs))
       (remove invalid-access?)))

(defn list-all-possible-paths
  [possible-dirs]
  (let [ini-paths (filter (fn [[start _]] (= "start" start)) possible-dirs)]
    (loop [exploring-paths ini-paths]
      (if (every? reach-end? exploring-paths)
        exploring-paths
        (recur (join-nex-possible-cave exploring-paths possible-dirs))))))

(comment
  ;;part1
  (let [possible-dirs (->> (puzzle-input example-data)
                          possible-directions)]
    (count (list-all-possible-paths possible-dirs)))

  ;; part1: 4775
  (count (list-all-possible-paths (possible-directions (puzzle-input)))))

