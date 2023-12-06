(require '[clojure.string :as s])

(def document (s/join "\n" ["Time:      7  15   30"
                            "Distance:  9  40  200"]))

(defn durations-records
  [document]
  (apply map vector
         (map (fn [line] (as-> line $
                           (re-matches #"^[^:]+:\s*(.*)$" $)
                           (last $)
                           (s/split $ #"\s+")
                           (map #(Integer/parseInt %) $)))
              (s/split-lines document))))

(defn n-wins
  [[time distance]]
  (->> (map #(* (- time %) %) (range 1 time))
       (filter #(< distance %))
       count))

(defn part1
  [document]
  (apply * (map n-wins (durations-records document))))

(let [document (slurp "06.txt")]
  (println (part1 document)))
