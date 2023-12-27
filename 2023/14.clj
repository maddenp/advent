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
      #_(println "@@@" (get line i) "i" i "x" x "load" load)
      #_(read-line)
      (if (< i n)
        (let [c (get line i) j (inc i)]
          (cond (= c \O)
                (recur j (inc x) (+ load (- n x)))
                (= c \#)
                (recur j j load)
                :else
                (recur j x load)))
        load))))

(defn part1
  [input]
  (let [lines (transpose (s/split input #"\n"))]
    (apply + (map score lines))))

(let [input #_input (slurp "14.txt")]
  (println (part1 input)))
