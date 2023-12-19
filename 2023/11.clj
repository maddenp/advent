(require '[clojure.math.combinatorics :refer [combinations]]
         '[clojure.string :as s])

(defn cols [arr] (alength (aget arr 0)))
(defn rows [arr] (alength arr))

(defn show
  [arr]
  (doseq [row (range (rows arr))]
    (doseq [col (range (cols arr))]
      (print (aget arr row col)))
    (println)))

(def input (s/join "\n" ["...#......"
                         ".......#.."
                         "#........."
                         ".........."
                         "......#..."
                         ".#........"
                         ".........#"
                         ".........."
                         ".......#.."
                         "#...#....."]))

(defn expand
  [image]
  (s/replace image #"(^|\n)(\.+\n)" "\n$2$2"))

(defn galaxies
  [image]
  (for [[r row] (map-indexed vector (s/split image #"\n"))
        [c sym] (map-indexed vector row)
        :when (= sym \#)]
    [r c]))

(defn manhattan-distance
  [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn transpose
  [s]
  (->> (s/split s #"\n")
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))
       (s/join "\n")))

(println
  (as-> input #_(slurp "11.txt") $
        (expand $)
        (transpose $)
        (expand $)
        (transpose $)
        (galaxies $)
        (combinations $ 2)
        (map (fn [[g1 g2]] (manhattan-distance g1 g2)) $)
        (apply + $)))
