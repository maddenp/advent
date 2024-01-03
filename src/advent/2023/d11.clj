(ns advent.2023.d11
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as s]))

(declare lines)

(defn expand
  [image]
  (s/replace image #"(^|\n)(\.+\n)" "\n$2$2"))

(defn galaxies
  [image]
  (for [[r row] (map-indexed vector (lines image))
        [c sym] (map-indexed vector row)
        :when (= sym \#)]
    [r c]))

(defn lines
  [image]
  (s/split image #"\n"))

(defn manhattan-distance
  [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn pairs
  [galaxies]
  (combinations galaxies 2))

(defn transpose
  [s]
  (->> (lines s)
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))
       (s/join "\n")))

(defn part1
  [image]
  (->> image
       expand
       transpose
       expand
       transpose
       galaxies
       pairs
       (map (fn [[g1 g2]] (manhattan-distance g1 g2)))
       (apply +)))

(defn part2
  [image]
  (let [walk #(apply range (sort [%1 %2]))
        dist #(if (re-matches #"^\.+$" %) 1000000 1)
        rdist (mapv dist (lines image))
        cdist (mapv dist (lines (transpose image)))
        gs (->> image galaxies sort pairs)]
    (apply + (flatten (for [[[r1 c1] [r2 c2]] gs]
                        [(map #(rdist %) (walk r1 r2))
                         (map #(cdist %) (walk c1 c2))])))))

(defn go
  [& args]
  (let [input (slurp (io/resource "resources/2023/d11.txt"))]
    [(part1 input) (part2 input)]))

(defn -main
  [& args]
  (apply println (go)))
