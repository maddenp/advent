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

#_(s/join "\n" (reverse (s/split pattern #"\n")))

(require '[clojure.pprint :refer [pprint]])

(defn f
  [c reject lines]
  #_(print c)
  #_(pprint lines)
  (if (seq lines)
    (let [n (count lines)]
      (if (= (mod n 2) 0)
        (do #_(print "a") #_(pprint (subvec lines 0 (/ n 2)))
            #_(print "b") #_(pprint (reverse (subvec lines (/ n 2))))
            (if (= (subvec lines 0 (/ n 2)) (reverse (subvec lines (/ n 2))))
              (+ c (/ n 2))
              (f (+ c 2) reject (vec (reject 2 lines))))
            )
        (f (+ c 1) reject (vec (reject 1 lines)))))
    0))

(def fwd (partial f 0 drop))
(def rev (partial f 0 drop-last))

(defn rows
  [pattern]
  (let [lines (s/split pattern #"\n")]
    (* 100 (+ (fwd lines) (rev lines)))))

(defn cols
  [pattern]
  (let [lines (s/split (transpose pattern) #"\n")]
    (+ (fwd lines) (rev lines))))

(defn score [pattern] (+ (rows pattern) (cols pattern)))

(defn part1
  [patterns]
  (apply + (map score patterns)))

(let [input #_input (slurp "13.txt")
      patterns (s/split input #"\n\n")]
  (println (part1 patterns)))
