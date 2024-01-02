(ns advent.2023.13
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as s]))

(declare alt-score alt-score-one cols npre-fwd npre-rev onediff? rows swap)

(defn alt
  [pattern]
  (let [lines (s/split pattern #"\n")]
    (as-> lines $
      (map-indexed #(hash-map %1 %2) $)
      (combinations $ 2)
      (remove #(apply = (vals (apply merge %))) $)
      (filter onediff? $)
      (map #(flatten (map keys %)) $)
      (map #(apply alt-score lines %) $)
      (filter #(seq %) $)
      (if (empty? $) 0 (first (vals (into {} $)))))))

(defn alt-score
  [lines a b]
  (let [aso (partial alt-score-one (rows (s/join "\n" lines)) lines)]
    (merge (aso a b) (aso b a))))

(defn alt-score-one
  [old lines j k]
  (let [s (->> (swap lines j k) (s/join "\n") rows)]
    (select-keys s (for [[k v] s :when (and (not= v (old k)) (not= 0 v))] k))))

(defn npre
  [coeff f pattern]
  (let [lines (s/split (f pattern) #"\n")]
    {:fwd (* coeff (npre-fwd lines)) :rev (* coeff (npre-rev lines))}))

(defn npre-fwd
  ([lines] (npre-fwd lines 0))
  ([lines n]
   (if (seq lines)
     (let [c (count lines)]
       (if (= (mod c 2) 0)
         (if (= (subvec lines 0 (/ c 2)) (reverse (subvec lines (/ c 2))))
           (+ n (/ c 2))
           (npre-fwd (vec (drop 2 lines))  (+ n 2)))
         (npre-fwd (vec (drop 1 lines)) (+ n 1))))
     0)))

(defn npre-rev
  [lines]
  (if (seq lines)
    (let [c (count lines)]
      (if (= (mod c 2) 0)
        (if (= (subvec lines 0 (/ c 2)) (reverse (subvec lines (/ c 2))))
          (/ c 2)
          (npre-rev (vec (drop-last 2 lines))))
        (npre-rev (vec (drop-last 1 lines)))))
    0))

(defn onediff?
  [pair]
  (= 1 (->> pair
            (map vals)
            flatten
            (apply map vector)
            (filter #(apply not= %))
            count)))

(defn score [pattern] {:rows (apply + (vals (rows pattern)))
                       :cols (apply + (vals (cols pattern)))})

(defn swap
  [lines j k]
  (for [[i line] (map-indexed vector lines)]
    (if (= i j) (nth lines k) line)))

(defn transpose
  [pattern]
  (->> (s/split pattern #"\n")
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))
       (s/join "\n")))

(def rows (partial npre 100 identity))
(def cols (partial npre 1 transpose))

(defn part1
  [patterns]
  (apply + (flatten (map vals (map score patterns)))))

(defn part2
  [patterns]
  (apply + (for [p patterns] (+ (alt p) (/ (alt (transpose p)) 100)))))

(defn go
  [& args]
  (let [input (slurp (io/resource "resources/2023/13.txt"))
        patterns (s/split input #"\n\n")]
    (println (part1 patterns) (part2 patterns))))
