#_(require '[clojure.pprint :only pprint])
(require '[clojure.string :as s])

(defn draw-maps
  [draw-str]
  (apply merge
         (map #(let [[n c] (s/split % #" ")] {c (Integer/parseInt n)})
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

(let [input (s/split (slurp "02.txt") #"\n")]
  (println (part1 input) #_(part2 input)))
