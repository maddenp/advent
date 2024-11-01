(ns advent.2023.d08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn prep
  [input]
  (let [[lr nodes] (s/split input #"\n\n")]
    (as-> (s/split nodes #"\n") $
      (map #(s/split % #" = ") $)
      (map #(hash-map (% 0) (zipmap [\L \R] (rest (re-matches #"^\((\w+), (\w+)\)$" (% 1))))) $)
      (apply merge $)
      [lr $])))

(defn get-cycle
  [lr nodes start]
  (let [pos #(mod % (count lr))
        next (fn [x n] ((nodes x) (nth lr (pos n))))]
    (loop [x start n 0 path [] seen #{}]
      (if (seen [x (pos n)])
        path
        (recur (next x n) (inc n) (conj path [x n]) (conj seen [x (pos n)]))))))

(defn part1
  [lr nodes]
  (loop [n 0 x "AAA"]
    (if (= x "ZZZ")
      n
      (recur (inc n) ((nodes x) (nth lr (mod n (count lr))))))))

(defn part2
  [lr nodes]
  (let [z? #(s/ends-with? (first %) "Z")]
    (->> (filter #(s/ends-with? % "A") (keys nodes))
         (map #(get-cycle lr nodes %))
         (map (fn [cycle] (last (first (filter z? cycle)))))
         (reduce lcm))))

(defn go
  [& _]
  (let [[lr nodes] (prep (slurp (io/resource "resources/2023/d08.txt")))]
    [(part1 lr nodes) (part2 lr nodes)]))

(defn -main
  [& _]
  (apply println (go)))
