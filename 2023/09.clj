(require '[clojure.string :as s])

(def input (s/join "\n" ["0 3 6 9 12 15"
                         "1 3 6 10 15 21"
                         "10 13 16 21 30 45"]))

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

(let [input #_input (slurp "09.txt")
      histories (as-> input $
                  (s/split $ #"\n")
                  (map #(s/split % #"\s+" ) $)
                  (map #(map (fn [x] (Long/parseLong x)) %) $))]
  (println (part1 histories)))
