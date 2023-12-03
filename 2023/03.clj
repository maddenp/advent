(require '[clojure.math.combinatorics :as c]
         '[clojure.string :as s])

(defn digit? [c] (<= (int \0) (int c) (int \9)))
(defn sym? [c] (not (or (digit? c) (= \. c))))
(defn at [a r c] (try (aget a r c) (catch Exception e \.)))
(defn neighbor-at [a r c [rd cd]] (at a (+ r rd) (+ c cd)))

(defn neighbors
  [a r c]
  (map #(neighbor-at a r c %)
       (c/cartesian-product [-1 0 +1] [-1 0 +1])))

(defn part1
  [input]
  (let [a (to-array-2d input)
        h (alength a)
        w (alength (aget a 0))
        runs (for [[r c] (c/cartesian-product (range h) (range w))]
               (let [x (at a r c)]
                 (when (digit? x)
                   {(some sym? (neighbors a r c)) x})))]
    (->> runs
         (partition-by some?)
         (filter #(some true? (flatten (map keys %))))
         (map #(Integer/parseInt (apply str (flatten (map vals %)))))
         (apply +))))

(let [input (s/split (slurp "03.txt") #"\n")]
  (println (part1 input)))

#_(println (part1 ["467..114.."
                 "...*......"
                 "..35..633."
                 "......#..."
                 "617*......"
                 ".....+.58."
                 "..592....."
                 "......755."
                 "...$.*...."
                 ".664.598.."])) ; 4361
