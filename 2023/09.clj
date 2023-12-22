(require '[clojure.string :as s])

(defn extrapolate
  [history]
  (if (every? zero? history)
    0
    (let [h (map (fn [[a b]] (- b a)) (partition 2 1 history))]
      (+ (last history) (extrapolate h)))))

(defn part1
  [histories]
  (apply + (map extrapolate histories)))

(defn part2
  [histories]
  (apply + (map extrapolate (map reverse histories))))

(let [histories (as-> (slurp "09.txt") $
                  (s/split $ #"\n")
                  (map #(s/split % #"\s+" ) $)
                  (map #(map (fn [x] (Long/parseLong x)) %) $))]
  (println (part1 histories) (part2 histories)))
