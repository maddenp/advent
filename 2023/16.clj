(require '[clojure.string :as s])

(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn show
  [a]
  (doseq [row (range (rows a))]
    (doseq [col (range (cols a))]
      (print (aget a row col)))
    (println)))

(def offsets {:n [-1 0] :e [0 +1] :s [+1 0] :w [0 -1]})

(defn visit
  [a r c dirs]
  (for [d' dirs
        :let [[dr dc] (offsets d') r' (+ r dr) c' (+ c dc)]
        :when (and (<= 0 r' (dec (rows a))) (<= 0 c' (dec (cols a))))]
    [r' c' d']))

(defn q'
  [seen q a r c dirs]
  (apply conj (rest q) (remove #(seen %) (visit a r c dirs))))

(defn dirs
  [a r c d]
  (case (aget a r c)
    \. [d]
    \| (case d :n [:n   ] :e [:n :s] :s [:s   ] :w [:n :s])
    \- (case d :n [:e :w] :e [:e   ] :s [:e :w] :w [:w   ])
    \/ (case d :n [:e   ] :e [:n   ] :s [:w   ] :w [:s   ])
    \\ (case d :n [:w   ] :e [:s   ] :s [:e   ] :w [:n   ])))

(defn energize
  [a]
  (let [energized (to-array-2d (repeat (rows a) (repeat (cols a) \.)))]
    (loop [seen #{} q (list [0 0 :e])]
      (if (seq q)
        (let [[r c d] (first q)]
          (aset energized r c \#)
          (recur (conj seen [r c d]) (q' seen q a r c (dirs a r c d))))
        energized))))

(defn part1
  [a]
  (let [energized (energize a)]
    (apply + (for [c (range (cols energized)) r (range (rows energized))]
               (if (= (aget energized r c) \#) 1 0)))))

(defn part2
  [a]
  a)

(let [a (to-array-2d (s/split (slurp "16.txt") #"\n"))]
  (println (part1 a) #_(part2 a)))
