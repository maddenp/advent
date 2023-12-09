(require '[clojure.string :as s])

(def input1 (s/join "\n" ["LLR"
                          ""
                          "AAA = (BBB, BBB)"
                          "BBB = (AAA, ZZZ)"
                          "ZZZ = (ZZZ, ZZZ)"]))

(defn prep
  [input]
  (let [[lr nodes] (s/split input #"\n\n")]
    (as-> (s/split nodes #"\n") $
      (map #(s/split % #" = ") $)
      (map #(hash-map (% 0) (zipmap [\L \R] (rest (re-matches #"^\((\w+), (\w+)\)$" (% 1))))) $)
      (apply merge $)
      [lr $])))

(defn part1
  [input]
  (let [[lr nodes] (prep input) m (count lr)]
    (loop [n 0 x "AAA"]
      (if (= x "ZZZ")
        n
        (recur (inc n) ((nodes x) (nth lr (mod n m))))))))

(defn part2
  [input]
  (let [[lr nodes] (prep input) m (count lr)]
    (loop [n 0 xs (filter #(s/ends-with? % "A") (keys nodes))]
      (if (every? #(s/ends-with? % "Z") xs)
        n
        (recur (inc n) (let [i (nth lr (mod n m))] (vec (map (fn [x] ((nodes x) i)) xs))))
        ))))

#_(let [input (slurp "08.txt")]
    (println (part2 input))
    #_(println (part1 input1) (part2 input2)))

(def input2 (s/join "\n" ["LR"
                          ""
                          "11A = (11B, XXX)"
                          "11B = (XXX, 11Z)"
                          "11Z = (11B, XXX)"
                          "22A = (22B, XXX)"
                          "22B = (22C, 22C)"
                          "22C = (22Z, 22Z)"
                          "22Z = (22B, 22B)"
                          "XXX = (XXX, XXX)"]))

;;       xs (filter #(s/ends-with? % "A") (keys nodes))]

(defn z? [s] (s/ends-with? s "Z"))

(defn get-cycle
  [lr nodes start]
  (let [m #(mod % (count lr))
        next (fn [x n] ((nodes x) (nth lr (m n))))]
    (loop [x start n 0 path [] seen #{}]
      (if (seen [x (m n)])
        path
        (recur (next x n) (inc n) (conj path [x n]) (conj seen [x (m n)]))))))

(let [[lr nodes] (prep input2 #_(slurp "08.txt"))
      cycle (get-cycle lr nodes "11A")
      a (last (last cycle))]
  (println (map #(vector a (last %)) (filter #(z? (first %)) cycle))))
