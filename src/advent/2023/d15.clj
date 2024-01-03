(ns advent.2023.d15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn h [s] (reduce #(mod (* 17 (+ %1 %2)) 256) 0 (map int s)))

(defn focusing-power
  [boxes]
  (for [[i lenses] boxes]
    (map-indexed #(* (inc i) (inc %1) (Integer/parseInt (last %2))) lenses)))

(defn lens-ins
  [boxes label foc-len]
  (let [i (h label) lenses (boxes i)]
    (assoc boxes i (vec (if (some #(= (first %) label) lenses)
                          (map #(if (= (first %) label) [label foc-len] %) lenses)
                          (conj lenses [label foc-len]))))))

(defn lens-rem
  [boxes label]
  (let [i (h label)]
    (assoc boxes i (vec (remove #(= (first %) label) (boxes i))))))

(defn part1 [steps] (apply + (map h steps)))

(defn part2
  [steps]
  (let [boxes (reduce merge (mapv #(hash-map % []) (range 256)))]
    (loop [steps steps boxes boxes]
      (if (seq steps)
        (let [[label foc-len] (s/split (first steps) #"[-=]")]
          (recur (rest steps) (if foc-len (lens-ins boxes label foc-len) (lens-rem boxes label))))
        (apply + (flatten (focusing-power boxes)))))))

(defn go
  [& args]
  (let [steps (s/split (s/trim-newline (slurp (io/resource "resources/2023/d15.txt"))) #",")]
    [(part1 steps) (part2 steps)]))

(defn -main
  [& args]
  (apply println (go)))
