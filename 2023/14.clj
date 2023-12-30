(require '[clojure.string :as s])

(defn transpose
  [lines]
  (->> lines
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))))

#_(defn weigh
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

(defn weigh
  [line]
  (apply + (map-indexed #(* (- (count line) %1) (if (= %2 \O) 1 0)) line)))

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

(defn tilt-e
  [lines]
  (map #(apply str (reverse %)) (map tilt (map #(apply str (reverse %)) lines))))

(defn tilt-n
  [lines]
  (transpose (map tilt (transpose lines))))

(defn tilt-s
  [lines]
  (reverse (transpose (map tilt (transpose (reverse lines))))))

(defn tilt-w
  [lines]
  (map tilt lines))

(def spin-cycle (comp tilt-e tilt-s tilt-w tilt-n))

#_(defn part1
  [input]
  (apply + (map weigh (transpose (s/split input #"\n")))))

(defn part1
  [input]
  (apply + (map weigh (transpose (tilt-n (s/split input #"\n"))))))

#_(defn part2
  [input]
  (let [lines (s/split input #"\n")]
    (loop [lines lines idx 0 idx2lines {} lines2idx {}]
      (let [next (spin-cycle lines)]
        (if-let [idx0 (lines2idx next)]
          (let [cyclen (- idx idx0)
                a (- 1000000000 idx0)
                b (mod a cyclen)]
            (part1 (s/join "\n" (idx2lines (+ idx0 b)))))
          (recur next (inc idx) (assoc idx2lines idx next) (assoc lines2idx next idx)))))))

(defn part2
  [input]
  (let [lines (s/split input #"\n")]
    (println 0 (apply + (map weigh (transpose lines))))
    (loop [lines lines idx 1]
      (let [next (spin-cycle lines)
            weight (apply + (map weigh (transpose next)))]
        (println idx weight)
        (read-line)
        (recur next (inc idx))))))

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

(require '[clojure.pprint :refer [pprint]])

(let [input #_input (slurp "14.txt")]
  (pprint (part1 input) #_(part2 input)))
