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
  [cards hand]
  (mapv #((zipmap cards (range 2 (+ 2 (count cards)))) %) hand))

(defn handcomp
  [h1 h2]
  (let [s1 (:strength h1) s2 (:strength h2)]
    (if (= s1 s2)
      (compare (:hand h1) (:hand h2))
      (compare s1 s2))))

(defn augment
  [cards hand bid]
  {:strength (strength hand) :hand (quantify cards hand) :bid (s->i bid)})

(defn part2
  [hands]
  (let [cards [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A]]
    (as-> (s/split hands #"\n") $
      (map #(s/split % #" ") $)
      (map #(augment cards (first %) (last %)) $)
      (sort handcomp $)
      (map vector (map #(:bid %) $) (range 1 (inc (count $))))
      (map #(apply * %) $)
      (apply + $))))

(defn part1
  [hands]
  (let [cards [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]]
    (as-> (s/split hands #"\n") $
      (map #(s/split % #" ") $)
      (map #(augment cards (first %) (last %)) $)
      (sort handcomp $)
      (map vector (map #(:bid %) $) (range 1 (inc (count $))))
      (map #(apply * %) $)
      (apply + $))))

(let [input (slurp "07.txt")]
  (println (part1 input)))
