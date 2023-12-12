(require '[clojure.string :as s])

(defn extrapolate
  [history]
  (if (every? zero? history)
    0
    (let [h (->> (rest history)
                 (interleave history)
                 (partition 2)
                 (map (fn [[a b]] (- b a))))]
      (+ (last history) (extrapolate h)))))

(defn part1
  [histories]
  (apply + (map extrapolate histories)))

(defn part2
  [histories]
  (apply + (map extrapolate (map reverse histories))))

(let [input (slurp "09.txt")
      histories (as-> input $
                  (s/split $ #"\n")
                  (map #(s/split % #"\s+" ) $)
                  (map #(map (fn [x] (Long/parseLong x)) %) $))]
  (println (part1 histories) (part2 histories)))
