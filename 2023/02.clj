(require '[clojure.pprint :only pprint])
(require '[clojure.string :as s])

(defn draw-maps
  [draw-str]
  (apply merge
         (map #(let [[n c] (s/split % #" ")] {(keyword c) (Integer/parseInt n)})
              (s/split draw-str #", "))))

(defn game-map
  [game-str]
  (let [[_ n draws] (re-matches #"^Game (\d+): (.*)$" game-str)]
    {:n (Integer/parseInt n)
     :draws (map draw-maps (s/split draws #"; "))}))

(defn valid-draw?
  [draw]
  (let [max {"red" 12 "green" 13 "blue" 14}]
    (every? true? (map #(<= (draw %) (max %)) (keys draw)))))

(defn valid-game?
  [game]
  (every? true? (map valid-draw? (:draws game))))

(defn part1
  [input]
  (apply + (map :n (filter valid-game? (map game-map input)))))

#_(let [input (s/split (slurp "02.txt") #"\n")]
  (println (part1 input) #_(part2 input)))

(defn color-max
  [draws]
  (let [colors [:red :green :blue]]
    (apply merge (map #(hash-map % (apply max (map (fn [d] (get d % 0)) draws))) colors))))

(defn power
  [draws]
  (apply * (vals (color-max draws))))

(color-max [{:red 4 :blue 3} {:red 1 :blue 6 :green 2} {:green 2}])
(power [{:red 4 :blue 3} {:red 1 :blue 6 :green 2} {:green 2}])
(apply + (map power (map :draws (map game-map input))))
#_(def input (s/split (slurp "02.txt") #"\n"))
#_(pprint (map game-map input))

#_(filter :x [{:a 1 :b 2} {:a 3 :b 4}])
