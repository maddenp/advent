(require '[clojure.string :as s])

(def input (s/join "\n" ["LLR"
                         ""
                         "AAA = (BBB, BBB)"
                         "BBB = (AAA, ZZZ)"
                         "ZZZ = (ZZZ, ZZZ)"]))

(defn part1
  [input]
  input)

(let [input input #_(slurp "08.txt")]
  (println (part1 input)))
