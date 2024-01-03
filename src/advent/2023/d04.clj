(ns advent.2023.d04
  (:require [clojure.java.io :as io]
            [clojure.math :refer [pow]]
            [clojure.set :refer [intersection]]
            [clojure.string :as s]))

(defn card-wins
  [card]
  (->> (re-matches #"^.*:\s*([\d ]+)\s*\|\s*([\d ]*)$" card)
       rest
       (map #(s/split % #"\s+"))
       (map #(set (map (fn [x] (Integer/parseInt x)) %)))
       (apply intersection)
       count))

(defn card-score [card] (int (pow 2 (dec (card-wins card)))))

(defn part1 [cards] (apply + (map card-score cards)))

(defn part2
  [cards]
  (loop [xs (repeat (count cards) 1) wins (map card-wins cards) total 0]
    (let [w (first wins) x (first xs)]
      (if (seq xs)
        (recur
          (concat (mapv + (repeat w x) (rest xs)) (drop (inc w) xs))
          (rest wins)
          (+ total x))
        total))))

(defn go
  [& _]
  (let [cards (s/split (slurp (io/resource "resources/2023/d04.txt")) #"\n")]
    [(part1 cards) (part2 cards)]))

(defn -main
  [& _]
  (apply println (go)))
