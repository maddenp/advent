(require '[clojure.string :as s])

(def input (s/join "\n" ["O....#...."
                         "O.OO#....#"
                         ".....##..."
                         "OO.#O....O"
                         ".O.....O#."
                         "O.#..O.#.#"
                         "..O..#O..O"
                         ".......O.."
                         "#....###.."
                         "#OO..#...."]))

(defn part1
  [input]
  input)

(let [input input #_(slurp "14.txt")]
  (println (part1 input)))
