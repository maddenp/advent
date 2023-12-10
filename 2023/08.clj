(require '[clojure.string :as s])

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn z? [s] (s/ends-with? s "Z"))

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
  (let [[lr nodes] (prep input)]
    (loop [n 0 x "AAA"]
      (if (= x "ZZZ")
        n
        (recur (inc n) ((nodes x) (nth lr (mod n (count lr)))))))))

(defn get-cycle
  [lr nodes start]
  (let [pos #(mod % (count lr))
        next (fn [x n] ((nodes x) (nth lr (pos n))))]
    (loop [x start n 0 path [] seen #{}]
      (if (seen [x (pos n)])
        path
        (recur (next x n) (inc n) (conj path [x n]) (conj seen [x (pos n)]))))))

(defn part2
  [input]
  (let [[lr nodes] (prep input)
      xs (filter #(s/ends-with? % "A") (keys nodes))
      cycles (map #(get-cycle lr nodes %) xs)
      lengths (map (fn [cycle] (last (first (filter #(z? (first %)) cycle)))) cycles)]
  (reduce lcm lengths)))

(let [input #_input2 (slurp "08.txt")]
    (println (part2 input))
    #_(println (part1 input1) (part2 input2)))

#_(defn get-params
  [lr nodes]
  (let [xs (filter #(s/ends-with? % "A") (keys nodes))
        cycles (map #(get-cycle lr nodes %) xs)]
    (for [cycle cycles]
      (let [cyclelen (last (last cycle))
            zs (filter #(z? (first %)) cycle)]
        (map #(vector cyclelen (last %)) zs)))))

#_(let [[lr nodes] (prep input2 #_(slurp "08.txt"))]
  (println (get-params lr nodes)))
;;   (let [pos #(mod % (count lr))
;;         next (fn [x n] ((nodes x) (nth lr (pos n))))]
;;     (loop [x "SVA" n 0 z0 0 z1 0]
;;       (when (z? x) (println "@@@" x n (- z1 z0)))
;;       (recur (next x n) (inc n) (if (z? x) z1 z0) (if (z? x) n z1)))))

