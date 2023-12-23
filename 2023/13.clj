(require '[clojure.string :as s])

(def input (s/join "\n" ["#.##..##."
                         "..#.##.#."
                         "##......#"
                         "##......#"
                         "..#.##.#."
                         "..##..##."
                         "#.#.##.#."
                         ""
                         "#...##..#"
                         "#....#..#"
                         "..##..###"
                         "#####.##."
                         "#####.##."
                         "..##..###"
                         "#....#..#"]))

(defn transpose
  [pattern]
  (->> (s/split pattern #"\n")
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))
       (s/join "\n")))

(defn fwd
  ([lines] (fwd lines 0))
  ([lines n]
   (if (seq lines)
     (let [c (count lines)]
       (if (= (mod c 2) 0)
         (if (= (subvec lines 0 (/ c 2)) (reverse (subvec lines (/ c 2))))
           (+ n (/ c 2))
           (fwd (vec (drop 2 lines))  (+ n 2)))
         (fwd (vec (drop 1 lines)) (+ n 1))))
     0)))

(defn rev
  [lines]
  (if (seq lines)
    (let [c (count lines)]
      (if (= (mod c 2) 0)
        (if (= (subvec lines 0 (/ c 2)) (reverse (subvec lines (/ c 2))))
          (/ c 2)
          (rev (vec (drop-last 2 lines))))
        (rev (vec (drop-last 1 lines)))))
    0))

(defn score
  [coeff f pattern]
  (let [lines (s/split (f pattern) #"\n")]
    (* coeff (+ (fwd lines) (rev lines)))))

(def rows (partial score 100 identity))
(def cols (partial score 1 transpose))

(defn part1
  [patterns]
  (apply + (map #(+ (rows %) (cols %)) patterns)))

(let [input #_input (slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (println (part1 patterns)))
