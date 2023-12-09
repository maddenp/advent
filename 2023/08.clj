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

;; (filter #(s/ends-with? % "A") (keys nodes))]

(defn z? [s] (s/ends-with? s "Z"))

(defn get-cycle
  [lr nodes start]
  (let [pos #(mod % (count lr))
        next (fn [x n] ((nodes x) (nth lr (pos n))))]
    (loop [x start n 0 path [] seen #{}]
      (if (seen [x (pos n)])
        path
        (recur (next x n) (inc n) (conj path [x n]) (conj seen [x (pos n)]))))))

(let [[lr nodes] (prep input2 #_(slurp "08.txt"))
      xs (filter #(s/ends-with? % "A") (keys nodes))
      cycles (map #(get-cycle lr nodes %) xs)]
  (doseq [cycle cycles]
    (println (map #(vector (last (last cycle)) (last %)) (filter #(z? (first %)) cycle)))))
