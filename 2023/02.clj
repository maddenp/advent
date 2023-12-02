(require '[clojure.string :as s])

(def colors [:red :green :blue])

(defn grab-maps
  [grab-str]
  (apply merge
         (map #(let [[n c] (s/split % #" ")] {(keyword c) (Integer/parseInt n)})
              (s/split grab-str #", "))))

(defn game-map
  [game-str]
  (let [[_ n grabs] (re-matches #"^Game (\d+): (.*)$" game-str)]
    {:n (Integer/parseInt n)
     :grabs (map grab-maps (s/split grabs #"; "))}))

(defn valid-grab?
  [grab]
  (let [max (zipmap colors [12 13 14])]
    (every? true? (map #(<= (grab %) (max %)) (keys grab)))))

(defn valid-game?
  [game]
  (every? true? (map valid-grab? (:grabs game))))

(defn color-max
  [grabs]
  (apply merge (for [c colors] {c (apply max (map #(get % c 0) grabs))})))

(defn power
  [grabs]
  (apply * (vals (color-max grabs))))

(defn part1
  [gm]
  (apply + (map :n (filter valid-game? gm))))

(defn part2
  [gm]
  (apply + (map power (map :grabs gm))))

(let [input (s/split (slurp "02.txt") #"\n")]
  (apply println (map #(% (map game-map input)) [part1 part2])))
