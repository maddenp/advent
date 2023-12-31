(require '[clojure.string :as s])

(defn cols [arr] (alength (aget arr 0)))
(defn rows [arr] (alength arr))

(defn show
  [arr]
  (doseq [row (range (rows arr))]
    (doseq [col (range (cols arr))]
      (print (aget arr row col)))
    (println)))

(defn part1
  [arr]
  arr)

(defn part2
  [arr]
  arr)

(let [arr (to-array-2d (s/split (slurp "16.tst") #"\n"))]
  (show (part1 arr) #_(part2 arr)))
