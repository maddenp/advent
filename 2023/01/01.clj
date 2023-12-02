(require '[clojure.string :as s])

(def numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn digit? [c] (<= (int \0) (int c) (int \9)))

(defn mark
  [word]
  (->> (for [[k v] (zipmap numbers (range (count numbers)))]
         (when (s/starts-with? word k)
           (apply str v (subs word 1))))
       (filter some?)
       first))

(defn mark-all
  [word]
  (loop [word word marked ""]
    (if (< (count word) 1)
      marked
      (let [w (or (mark word) word)]
        (recur (subs w 1) (str marked (first w)))))))

(defn part1
  [text]
  (let [f #(Integer/parseInt (let [d (filter digit? %)] (str (first d) (last d))))]
    (apply + (map f (s/split text #"\n")))))

(defn part2
  [text]
  (part1 (s/join "\n" (map mark-all (s/split text #"\n")))))

(let [input (slurp "input.txt")]
  (println (part1 input) (part2 input)))
