(require '[clojure.math.combinatorics :refer [combinations]]
         '[clojure.string :as s])

(require '[clojure.pprint :refer [pprint]]) ; TODO REMOVE

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

(defn npre
  [coeff f pattern]
  (let [lines (s/split (f pattern) #"\n")]
    (* coeff (+ (fwd lines) (rev lines)))))

#_(defn npre
  [coeff f pattern]
  (let [lines (s/split (f pattern) #"\n")
        npre-fwd (fwd lines)
        npre-rev (rev lines)]
    (println "@@@ npre-fwd" npre-fwd "npre-rev" npre-rev)
    (* coeff (+ npre-fwd npre-rev))))

(def rows (partial npre 100 identity))
(def cols (partial npre 1 transpose))

#_(defn score [pattern] (+ (rows pattern) (cols pattern)))
(defn score [pattern] {:rows (rows pattern) :cols (cols pattern)})

(defn part1
  [patterns]
  (apply + (flatten (map vals (map score patterns)))))

(defn onediff?
  [pair]
  (= 1 (->> pair
            (map vals)
            flatten
            (apply map vector)
            (filter #(apply not= %))
            count)))

(defn alt-score-rows
  [lines j k]
  (println j "->"  k)
  (pprint (s/join "\n" lines))
  (println "--")
  (let [alt (for [[i line] (map-indexed vector lines)]
              (if (= i j) (nth lines k) line))]
    (let [pattern (s/join "\n" alt)
          s (rows pattern)]
      (pprint pattern)
      (println s)
      s)))

(defn alt-score
  [lines a b]
  (println)
  [(alt-score-rows lines a b) (alt-score-rows lines b a)])

(defn part2
  [patterns]
  (for [pattern [(nth patterns 1)]]
    (let [;old-score (score pattern)
          lines (s/split pattern #"\n")]
      (as-> lines $
        (map-indexed #(hash-map %1 %2) $)
        (combinations $ 2)
        (remove #(apply = (vals (apply merge %))) $)
        (filter onediff? $)
        (map #(flatten (map keys %)) $)
        (map #(apply alt-score lines %) $)
        ))))

(let [input #_input (slurp "13.txt")
      patterns (s/split input #"\n\n")]
  #_(doseq [x (part2 patterns)] (println x))
  (println (part1 patterns) #_(part2 patterns)))
