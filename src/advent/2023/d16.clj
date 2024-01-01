(ns advent.2023.d16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn dirs
  [a r c d]
  (case (aget a r c)
    \. [d]
    \| (case d :n [:n   ] :e [:n :s] :s [:s   ] :w [:n :s])
    \- (case d :n [:e :w] :e [:e   ] :s [:e :w] :w [:w   ])
    \/ (case d :n [:e   ] :e [:n   ] :s [:w   ] :w [:s   ])
    \\ (case d :n [:w   ] :e [:s   ] :s [:e   ] :w [:n   ])))

(defn visit
  [a r c d]
  (let [offsets {:n [-1 0] :e [0 +1] :s [+1 0] :w [0 -1]}]
    (for [d' (dirs a r c d)
          :let [[dr dc] (offsets d') r' (+ r dr) c' (+ c dc)]
          :when (and (<= 0 r' (dec (rows a))) (<= 0 c' (dec (cols a))))]
      [r' c' d'])))

(defn q'
  [seen q a r c d]
  (apply conj (rest q) (remove #(seen %) (visit a r c d))))

(defn energize
  [a r c d]
  (loop [q (list [r c d]) energized #{} seen #{}]
    (if (seq q)
      (let [[r c d] (first q)]
        (recur (q' seen q a r c d) (conj energized [r c]) (conj seen [r c d])))
      (count energized))))

(defn part1
  [a]
  (energize a 0 0 :e))

(defn part2
  [a]
  (let [ra (rows a) ca (cols a) rs (range ra) cs (range ca)]
    (->> (for [[r c d] (concat (map #(vector 0 % :s) cs)
                               (map #(vector % (dec ca) :w) (rest rs))
                               (map #(vector (dec ra) % :n) (rest (reverse cs)))
                               (map #(vector % 0 :e) (rest (reverse (rest rs)))))]
           (energize a r c d))
         (apply max))))

(defn go
  [& args]
  (let [a (to-array-2d (s/split (slurp (io/resource "resources/2023/d16.txt")) #"\n"))]
    [(part1 a) (part2 a)]))

#_(defn show
    [a]
    (doseq [row (range (rows a))]
      (doseq [col (range (cols a))]
        (print (aget a row col)))
      (println)))
