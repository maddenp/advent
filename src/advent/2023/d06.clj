(ns advent.2023.06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn s->i [s] (Long/parseLong s))

(defn n-wins
  [[time distance]]
  (->> (map #(* (- time %) %) (range 1 time))
       (filter #(< distance %))
       count))

(defn part1
  [numstrs]
  (->> (map #(map s->i (s/split % #"\s+")) numstrs)
       (apply map vector)
       (map n-wins)
       (apply *)))

(defn part2
  [numstrs]
  (->> (map #(s->i (s/replace % #"\s+" "")) numstrs)
       n-wins))

(defn go
  [& args]
  (let [input (s/split-lines (slurp (io/resource "resources/2023/06.txt")))
        numstrs (map #(last (re-matches #"^[^:]+:\s*(.*)$" %)) input)]
    (println (part1 numstrs) (part2 numstrs))))
