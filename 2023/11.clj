(require '[clojure.math.combinatorics :refer [cartesian-product]]
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

(defn transpose
  [s]
  (->> (s/split s #"\n")
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))
       (s/join "\n")))

(defn expand [s] (s/replace s #"(^|\n)(\.+\n)" "\n$2$2"))

(println (as-> input #_(slurp "11.txt") $
               (expand $)
               (transpose $)
               (expand $)
               (transpose $)
               #_(to-array-2d $)
               #_(show $)
               ))
