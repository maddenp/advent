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
  ([lines c]
   (if (seq lines)
     (let [n (count lines)]
       (if (= (mod n 2) 0)
         (if (= (subvec lines 0 (/ n 2)) (reverse (subvec lines (/ n 2))))
           (+ c (/ n 2))
           (fwd (vec (drop 2 lines))  (+ c 2)))
         (fwd (vec (drop 1 lines)) (+ c 1))))
     0)))

(defn rev
  [lines]
  (if (seq lines)
    (let [n (count lines)]
      (if (= (mod n 2) 0)
        (if (= (subvec lines 0 (/ n 2)) (reverse (subvec lines (/ n 2))))
          (/ n 2)
          (rev (vec (drop-last 2 lines))))
        (rev (vec (drop-last 1 lines)))))
    0))

(defn rows
  [pattern]
  (let [lines (s/split pattern #"\n")]
    (* 100 (+ (fwd lines)
              (if (= lines (reverse lines)) 0 (rev lines))))))

(defn cols
  [pattern]
  (let [lines (s/split (transpose pattern) #"\n")]
    (+ (fwd lines)
       (if (= lines (reverse lines)) 0 (rev lines)))))

(defn part1
  [patterns]
  (apply + (map #(+ (rows %) (cols %)) patterns)))

(let [input #_input (slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (println (part1 patterns)))
