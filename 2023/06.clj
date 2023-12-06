(require '[clojure.string :as s])

(defn n-wins
  [[time distance]]
  (->> (map #(* (- time %) %) (range 1 time))
       (filter #(< distance %))
       count))

(defn part1
  [document]
  (let [pairs (apply map vector
                     (map (fn [line] (as-> line $
                                       (re-matches #"^[^:]+:\s*(.*)$" $)
                                       (last $)
                                       (s/split $ #"\s+")
                                       (map #(Integer/parseInt %) $)))
                          (s/split-lines document)))]
    (apply * (map n-wins pairs))))

(defn part2
  [document]
  (let [[t d] (map (fn [line] (as-> line $
                                (re-matches #"^[^:]+:\s*(.*)$" $)
                                (last $)
                                (s/replace $ #"\s+" "")
                                (Long/parseLong $)))
                   (s/split-lines document))]
    (n-wins [t d])))

(let [document (slurp "06.txt")]
  (println (part1 document) (part2 document)))

(comment
  (def document
    (s/join "\n" ["Time:      7  15   30"
                  "Distance:  9  40  200"])))
