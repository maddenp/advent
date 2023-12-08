(require '[clojure.string :as s])

(def input (s/join "\n" ["LLR"
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
    (loop [n 0 node "AAA"]
      (if (= node "ZZZ")
        n
        (recur (inc n) ((nodes node) (nth lr (mod n m))))))))

(let [input #_input (slurp "08.txt")]
  (println (part1 input)))
