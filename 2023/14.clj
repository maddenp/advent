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

(defn transpose
  [lines]
  (->> lines
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))))

(defn score
  [line]
  (let [n (count line)]
    (loop [i 0 x 0 load 0]
      (if (< i n)
        (let [j (inc i)]
          (case (get line i)
            \O (recur j (inc x) (+ load (- n x)))
            \# (recur j j load)
            (recur j x load)))
        load))))

(defn part1
  [input]
  (let [lines (transpose (s/split input #"\n"))]
    (apply + (map score lines))))

(let [input #_input (slurp "14.txt")]
  (println (part1 input)))
