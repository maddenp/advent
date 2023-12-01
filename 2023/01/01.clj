(require '[clojure.string :as s])

(def input (slurp "input.txt"))
(def numbers (zipmap ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] (range 10)))
(defn digit? [c] (<= (int \0) (int c) (int \9)))

(defn crunch-one
  [word]
  (->> (for [[k v] numbers]
         (when (s/starts-with? word k)
           (apply str v (subs word 1))))
       (filter some?)
       first))

(defn crunch
  [word]
  (loop [word word new ""]
    (if (< (count word) 1)
      new
      (let [word (or (crunch-one word) word)]
        (recur (subs word 1) (str new (first word)))))))

(defn part1
  [lines]
  (as-> lines $
    (s/split $ #"\n")
    (map #(filter digit? %) $)
    (map #(list (first %) (last %)) $)
    (map #(apply str %) $)
    (map #(Integer/parseInt %) $)
    (apply + $)))

(defn part2
  [input]
  (part1 (s/join "\n" (map crunch (s/split input #"\n")))))

(part1 input)
(part2 input)
