(ns advent.2023.d16
  (:require [advent.common :refer [cols rows show]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [ubergraph.alg :refer [shortest-path]]
            [ubergraph.core :refer [digraph]]))

(defn dirs
  [a r c d]
  (case (aget a r c)
    \. [d]
    \| (case d :n [:n   ] :e [:n :s] :s [:s   ] :w [:n :s])
    \- (case d :n [:e :w] :e [:e   ] :s [:e :w] :w [:w   ])
    \/ (case d :n [:e   ] :e [:n   ] :s [:w   ] :w [:s   ])
    \\ (case d :n [:w   ] :e [:s   ] :s [:e   ] :w [:n   ])))

(defn neighbors
  [a r c d]
  (let [offsets {:n [-1 0] :e [0 +1] :s [+1 0] :w [0 -1]}]
    (for [d' (dirs a r c d)
          :let [[dr dc] (offsets d') r' (+ r dr) c' (+ c dc)]
          :when (and (<= 0 r' (dec (rows a))) (<= 0 c' (dec (cols a))))]
      [r' c' d'])))

(defn tally
  [g node]
  (count (set (map #(take 2 %) (keys (:depths (shortest-path g {:start-node node})))))))

(defn part1
  [g]
  (tally g [0 0 :e]))

(defn part2
  [g a]
  (let [ra (rows a) ca (cols a) rs (range ra) cs (range ca)]
    (->> (for [[r c d] (concat (map #(vector 0 % :s) cs)
                               (map #(vector % (dec ca) :w) (rest rs))
                               (map #(vector (dec ra) % :n) (rest (reverse cs)))
                               (map #(vector % 0 :e) (rest (reverse (rest rs)))))]
           (tally g [r c d]))
         (apply max))))

(defn go
  [& _]
  (let [a (to-array-2d (s/split (slurp (io/resource "resources/2023/d16.txt")) #"\n"))
        g (apply digraph
                 (for [r (range (rows a))
                       c (range (cols a))
                       d [:n :e :s :w]
                       n (neighbors a r c d)]
                   [[r c d] n]))]
    [(part1 g) (part2 g a)]))

(defn -main
  [& _]
  (apply println (go)))
