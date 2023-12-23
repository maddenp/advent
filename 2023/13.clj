(require '[clojure.math.combinatorics :refer [combinations]]
         '[clojure.string :as s])

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

(require '[clojure.pprint :refer [pprint]])

(defn onediff?
  [pair]
  (= 1 (->> pair
            (map vals)
            flatten
            (apply map vector)
            (filter #(apply not= %))
            count)))

(defn part2
  [patterns]
  (for [pattern (rest patterns)]
    (let [old-score (+ (rows pattern) (cols pattern))
          lines (s/split pattern #"\n")]
      (as-> lines $
        (map-indexed #(hash-map %1 %2) $)
        (combinations $ 2)
        (remove #(apply = (vals (apply merge %))) $)
        (filter onediff? $)))))

(+ 1 1)
(let [input input #_(slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (println #_(part1 patterns) (part2 patterns)))
