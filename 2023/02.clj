(require '[clojure.string :as s])

(def colors [:red :green :blue])

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
  (let [max (zipmap colors [12 13 14])]
    (every? true? (map #(<= (draw %) (max %)) (keys draw)))))

(defn valid-game?
  [game]
  (every? true? (map valid-draw? (:draws game))))

(defn color-max
  [draws]
  (apply merge (for [c colors] {c (apply max (map #(get % c 0) draws))})))

(defn power
  [draws]
  (apply * (vals (color-max draws))))

(defn part1
  [gm]
  (apply + (map :n (filter valid-game? gm))))

(defn part2
  [gm]
  (apply + (map power (map :draws gm))))

(let [input (s/split (slurp "02.txt") #"\n")]
  (apply println (map #(% (map game-map input)) [part1 part2])))
