(require '[clojure.math :refer [pow]]
         '[clojure.set :refer [intersection]]
         '[clojure.string :as s])

(def cards ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn wins
  [card]
  (->> (re-matches #"^.*:\s*([\d ]+)\s*\|\s*([\d ]*)$" card)
       rest
       (map #(s/split % #"\s+"))
       (map #(set (map (fn [x] (Integer/parseInt x)) %)))
       (apply intersection)
       count))

(defn score-card
  [card]
  (int (pow 2 (dec (wins card)))))

(defn part1
  [cards]
  (apply + (map score-card cards)))

(let [cards (s/split (slurp "04.txt") #"\n")]
  (println (part1 cards) #_(part2 cards)))

;; (defn part2
;;   [cards]
;;   (for [card in cards]
;;     (->> (re-matches #"^.*:\s*([\d ]+)\s*\|\s*([\d ]*)$" card)
;;          rest
;;          (map #(s/split % #"\s+"))
;;          (map #(set (map (fn [x] (Integer/parseInt x)) %)))
;;          (apply intersection)
;;          count
;;          dec
;;          (pow 2)
;;          int

#_(part2 cards)
