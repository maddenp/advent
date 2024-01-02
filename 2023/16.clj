(ns advent.2023.16
  (:require [clojure.string :as s]))

(def offsets {:n [-1 0] :e [0 +1] :s [+1 0] :w [0 -1]})

(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn show
  [a]
  (doseq [row (range (rows a))]
    (doseq [col (range (cols a))]
      (print (aget a row col)))
    (println)))

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
  (for [d' (dirs a r c d)
        :let [[dr dc] (offsets d') r' (+ r dr) c' (+ c dc)]
        :when (and (<= 0 r' (dec (rows a))) (<= 0 c' (dec (cols a))))]
    [r' c' d']))

(defn q'
  [seen q a r c d]
  (apply conj (rest q) (remove #(seen %) (visit a r c d))))

(defn energize
  [a r c d]
  (let [energized (to-array-2d (repeat (rows a) (repeat (cols a) \.)))]
    (loop [seen #{} q (list [r c d])]
      (if (seq q)
        (let [[r c d] (first q)]
          (aset energized r c \#)
          (recur (conj seen [r c d]) (q' seen q a r c d)))
        energized))))

(defn tally
  [a]
  (apply + (for [c (range (cols a)) r (range (rows a))]
             (if (= (aget a r c) \#) 1 0))))

(defn part1
  [a]
  (tally (energize a 0 0 :e)))

(defn part2
  [a]
  (let [ra (rows a) ca (cols a) rs (range ra) cs (range ca)]
    (->> (for [[r c d] (concat (mapv vector (repeat ca 0) cs (repeat ca :s))
                               (mapv vector (rest rs) (repeat ra (dec ca)) (repeat ra :w))
                               (mapv vector (repeat ca (dec ra)) (rest (reverse cs)) (repeat ca :n))
                               (mapv vector (rest (reverse (rest rs))) (repeat ca 0) (repeat ra :e)))]
           (tally (energize a r c d)))
         (apply max))))

(defn go
  []
  (let [a (to-array-2d (s/split (slurp "16.txt") #"\n"))]
    (println (part1 a) (part2 a))))

(go)
