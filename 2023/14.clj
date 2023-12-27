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

(defn weigh
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

(defn swap
  [s a b]
  (if (= a b)
    s
    (let [[a b] (sort [a b])]
      (str (subs s 0 a) (get s b) (subs s (inc a) b) (get s a) (subs s (inc b))))))

(defn tilt
  [line]
  (let [n (count line)]
    (loop [i 0 x 0 s line]
      (if (< i n)
        (let [j (inc i)]
          (case (get line i)
            \O (recur j (inc x) (swap s i x))
            \# (recur j j s)
            (recur j x s)))
        s))))

(defn part1
  [input]
  (let [lines (transpose (s/split input #"\n"))]
    (apply + (map weigh lines))))

(defn part2
  [input]
  (let [lines (s/split input #"\n")]
    (s/join "\n" (transpose (map tilt (transpose lines))))))

(let [input input #_(slurp "14.txt")]
  (println #_(part1 input) (part2 input)))
