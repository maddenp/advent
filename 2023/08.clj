(require '[clojure.string :as s])

(def input1 (s/join "\n" ["LLR"
                          ""
                          "AAA = (BBB, BBB)"
                          "BBB = (AAA, ZZZ)"
                          "ZZZ = (ZZZ, ZZZ)"]))

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
    (let [[lr nodes] (prep input) xs (filter #(s/ends-with? % "A") (keys nodes))]
      xs
      ))

(require '[clojure.pprint :refer [pprint]])
(let [input (slurp "08.txt")]
  (pprint (part2 input2))
  #_(println (part1 input1) (part2 input2)))
