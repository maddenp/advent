(require '[clojure.math :refer [pow]]
         '[clojure.set :refer [intersection]]
         '[clojure.string :as s])

(def cards ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn card-wins
  [card]
  (->> (re-matches #"^.*:\s*([\d ]+)\s*\|\s*([\d ]*)$" card)
       rest
       (map #(s/split % #"\s+"))
       (map #(set (map (fn [x] (Integer/parseInt x)) %)))
       (apply intersection)
       count))

(defn card-score
  [card]
  (int (pow 2 (dec (card-wins card)))))

(defn part1
  [cards]
  (apply + (map card-score cards)))

(defn part2
  [cards]
  (loop [xs (repeat (count cards) 1) wins (map card-wins cards) total 0]
    (if (seq xs)
      (recur
        (concat (mapv + (repeat (first wins) (first xs)) (rest xs)) (drop (inc (first wins)) xs))
        (rest wins)
        (+ total (first xs)))
      total)))

(let [cards (s/split (slurp "04.txt") #"\n")]
  (println (part1 cards) (part2 cards)))

#_(part2 cards)
#_(let [xs [1 1 1 1 1 1] wins [4 2 2 1 0 0]]
  (concat (mapv + (repeat (first wins) 1) (rest xs)) (drop (first wins) xs)))
