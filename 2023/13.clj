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
    {:fwd (* coeff (fwd lines)) :rev (* coeff (rev lines))}))

(def rows (partial npre 100 identity))
(def cols (partial npre 1 transpose))

(defn score [pattern] {:rows (apply + (vals (rows pattern)))
                       :cols (apply + (vals (cols pattern)))})

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
  [old-rows lines j k]
  #_(println "old-rows" old-rows "|" j "->"  k)
  #_(pprint (s/join "\n" lines))
  #_(println "--")
  (let [alt (for [[i line] (map-indexed vector lines)]
              (if (= i j) (nth lines k) line))]
    (let [pattern (s/join "\n" alt)
          s (rows pattern)]
      #_(pprint pattern)
      #_(println "s" s)
      (let [t (select-keys s (for [[k v] s :when (and (not= v (old-rows k)) (not= 0 v))] k))]
        #_(println "t" t)
        t))))

(defn alt-score
  [lines a b]
  (let [pattern (s/join "\n" lines)
        old-rows (rows pattern)]
    (let [ab (alt-score-rows old-rows lines a b)
          ba (alt-score-rows old-rows lines b a)
          m (merge ab ba)]
      #_(println "@@@ ab" ab)
      #_(println "@@@ ba" ba)
      #_(println "@@@ m" m)
      m)))
      #_(if (seq m)
        (->> m
             vals
             (filter #(not= % 0))
             first
             )
        m)

(defn part2
  [patterns]
  (for [pattern [(nth patterns 1)]]
    (let [lines (s/split pattern #"\n")
          foo (as-> lines $
                (map-indexed #(hash-map %1 %2) $)
                (combinations $ 2)
                (remove #(apply = (vals (apply merge %))) $)
                (filter onediff? $)
                (map #(flatten (map keys %)) $)
                (map #(apply alt-score lines %) $)
                (into {} $)
                (vals $)
                (first $)
                #_(set $)
                #_(first $)
                )]
      (println "@@@ foo" foo "!")
      foo)))

#_(let [input (slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (println (part1 patterns) #_(part2 patterns)))

(let [input input #_(slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (part2 patterns))
