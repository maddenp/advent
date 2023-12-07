(require '[clojure.string :as s])

(def hands (s/join "\n" ["32T3K 765"
                         "T55J5 684"
                         "KK677 28"
                         "KTJJT 220"
                         "QQQJA 483"]))

(defn s->i [s] (Long/parseLong s))

(defn strength
  [hand]
  (let [s {[5] 7 [1 4] 6 [2 3] 5 [1 1 3] 4 [1 2 2] 3 [1 1 1 2] 2 [1 1 1 1] 1}]
    (s (vec (sort (vals (frequencies hand)))))))

(defn quantify
  [hand]
  (let [cards [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]]
    (map #((zipmap cards (range 2 (+ 2 (count cards)))) %) hand)))

(as-> (s/split hands #"\n") $
  (map #(s/split % #" ") $)
  (map (fn [[hand bid]] [(quantify hand) (s->i bid)]) $)
  )

(defn handnum
  [hand]
  
(sort '(3 2 10 3 13) '(10 5 5 11 5))
