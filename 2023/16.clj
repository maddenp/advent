(require '[clojure.string :as s])

(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn enqueue
  [q a r c dirs]
  (let [offsets {:n [-1 0] :e [0 +1] :s [+1 0] :w [0 -1]}
        xs (for [d' dirs
                 :let [[dr dc] (offsets d') r' (+ r dr) c' (+ c dc)]
                 :when (and (<= 0 r' (dec (rows a))) (<= 0 c' (dec (cols a))))]
             [r' c' d'])]
    (apply conj (rest q) xs)))

(defn show ; TODO REMOVE
  [a]
  (doseq [row (range (rows a))]
    (doseq [col (range (cols a))]
      (print (aget a row col)))
    (println)))

(defn part1
  [a]
  (let [energized (to-array-2d (repeat (rows a) (repeat (cols a) \.)))]
    (loop [q (list [0 0 :e])]
      (if (seq q)
        (let [[r c d] (first q) visit (partial enqueue q a r c)]
          (show a)
          (println "r" r "c" c "d" d "q" q)
          (aset energized r c \#)
          (show energized)
          (read-line)
          (case (aget a r c)
            \. (recur (visit [d]))
            \| (recur (visit (case d :n [:n] :e [:n :s] :s [:s] :w [:n :s])))
            \- (recur (visit (case d :n [:e :w] :e [:e] :s [:e :w] :w [:w])))
            \/ (recur (visit (case d :n [:e] :e [:n] :s [:w] :w [:s])))
            \\ (recur (visit (case d :n [:w] :e [:s] :s [:e] :w [:n])))))
        energized))))

(defn part2
  [a]
  a)

(let [a (to-array-2d (s/split (slurp "16.tst") #"\n"))]
  (println (part1 a) #_(part2 a)))
