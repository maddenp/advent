(require '[clojure.string :as s])

(defn digit? [c] (<= (int \0) (int c) (int \9)))

(as-> (slurp "input.txt") $
  (s/split $ #"\n")
  (map #(filter digit? %) $)
  (map #(list (first %) (last %)) $)
  (map #(apply str %) $)
  (map #(Integer/parseInt %) $)
  (apply + $))
