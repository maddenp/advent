(require '[clojure.string :as s])

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
  (let [a-nodes (filter #(s/ends-with? % "A") (keys nodes))
        cycles (map #(get-cycle lr nodes %) a-nodes)
        lengths (map (fn [cycle] (last (first (filter #(s/ends-with? (first %) "Z") cycle)))) cycles)]
    (reduce lcm lengths)))

(let [[lr nodes] (prep (slurp "08.txt"))]
    (println (part1 lr nodes) (part2 lr nodes)))
