(require '[clojure.math.combinatorics :refer [cartesian-product] :rename {cartesian-product prod}]
         '[clojure.string :as s])

(defn digit? [c] (<= (int \0) (int c) (int \9)))
(defn sym? [c] (not (or (digit? c) (= \. c))))
(defn at [a r c] (try (aget a r c) (catch Exception e \.)))

(defn neighbors
  [a r c]
  (map (fn [[rd cd]] (at a (+ r rd) (+ c cd)))
       (prod [-1 0 +1] [-1 0 +1])))

(defn rows [a] (alength a))
(defn cols [a] (alength (aget a 0)))

(defn halo
  [a r c]
  (->> (prod [-1 0 +1] [-1 0 +1])
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter (fn [[hr hc]] (and (not= [hr hc] [r c])
                                  (< 0 hr (rows a))
                                  (< 0 hc (cols a)))))))

(defn digits
  [a]
  (let [pred digit?
        coords (prod (range (rows a)) (range (cols a)))
        select (fn [[r c]] (when (pred (at a r c)) {:rc [r c] :halo (halo a r c)}))]
    (filter some? (map select coords))))

#_(defn part1
    [input]
    (let [a (to-array-2d input)
          h (alength a)
          w (alength (aget a 0))
          runs (for [[r c] (prod (range h) (range w))]
                 (let [x (at a r c)]
                   (when (digit? x)
                     {(some sym? (neighbors a r c)) x})))]
      (->> runs
           (partition-by some?)
           (filter #(some true? (flatten (map keys %))))
           (map #(Integer/parseInt (apply str (flatten (map vals %)))))
           (apply +))))

(defn part1
  [a]
  (loop [ds (digits a) ns [] n nil] ; use 0?
    (if (seq ds)
      (let [d (first ds) [r c] (:rc d) [nr nc] (:rc (last n))] ; use 0?
        (println d r c n nr nc)
        (cond
          (nil? n) (recur (rest ds) ns [d])
          (and (= r nr) (= c (inc nc))) (recur (rest ds) ns (conj n d))
          :else (recur (rest ds) (conj ns n) [d])))
      ns)))

(require '[clojure.pprint :refer [pprint]])
(pprint (part1 (to-array-2d ["467..114.."
                             "...*......"
                             "..35..633."
                             "......#..."
                             "617*......"
                             ".....+.58."
                             "..592....."
                             "......755."
                             "...$.*...."
                             ".664.598.."]))) ; 4361

#_(let [input (s/split (slurp "03.txt") #"\n")]
    (println (part1 input)))
