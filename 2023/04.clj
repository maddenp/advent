(require '[clojure.math :refer [pow]]
         '[clojure.set :refer [intersection]]
         '[clojure.string :as s])

(defn card-wins
  [card]
  (->> (re-matches #"^.*:\s*([\d ]+)\s*\|\s*([\d ]*)$" card)
       rest
       (map #(s/split % #"\s+"))
       (map #(set (map (fn [x] (Integer/parseInt x)) %)))
       (apply intersection)
       count))

(defn card-score [card] (int (pow 2 (dec (card-wins card)))))

(defn part1 [cards] (apply + (map card-score cards)))

(defn part2
  [cards]
  (loop [xs (repeat (count cards) 1) wins (map card-wins cards) total 0]
    (let [w (first wins) x (first xs)]
      (if (seq xs)
        (recur
          (concat (mapv + (repeat w x) (rest xs)) (drop (inc w) xs))
          (rest wins)
          (+ total x))
        total))))

(let [cards (s/split (slurp "04.txt") #"\n")]
  (println (part1 cards) (part2 cards)))
