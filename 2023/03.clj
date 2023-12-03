(require '[clojure.math.combinatorics :refer [cartesian-product] :rename {cartesian-product prod}]
         '[clojure.set :refer [difference]]
         '[clojure.string :as s])

(declare runs select)

(defn at [a [r c]] (try (aget a r c) (catch Exception _ \.)))
(defn cols [a] (alength (aget a 0)))
(defn digit? [c] (<= (int \0) (int c) (int \9)))
(defn rows [a] (alength a))
(defn sym? [c] (not (or (digit? c) (= \. c))))

(defn halo
  "The coordinates of all in-bounds neighbors."
  [a [r c]]
  (->> (prod [-1 0 +1] [-1 0 +1])
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter (fn [[hr hc]] (and (not= [hr hc] [r c])
                                  (<= 0 hr (dec (rows a)))
                                  (<= 0 hc (dec (cols a))))))))

(defn numbers
  "Numbers from the input along with adjacent chars."
  [a]
  (for [run (runs a)]
    (let [rcs (map :rc run)]
      {:n (->> (map #(at a %) rcs)         ; coords -> chars
               (apply str)                 ; combine chars -> str
               Integer/parseInt)           ; str -> int
       :adj (as-> (map :halo run) $        ; get all neighbor coords
              (reduce into [] $)           ; all coords in one vector
              (set $)                      ; remove duplicates
              (difference $ (set rcs))     ; remove coords belonging to number
              (map #(at a %) $)            ; coords -> chars
              (set $)                      ; remove duplicates
              (filter #(not= \. %) $))}))) ; remove '.'s

(defn runs
  "Groups of runs of successive digits in the same row."
  [a]
  (loop [ds (select a digit?) ns [] n nil]
    (if (seq ds)
      (let [d (first ds) [r c] (:rc d) [nr nc] (:rc (last n))]
        (cond
          (nil? n) (recur (rest ds) ns [d])
          (and (= r nr) (= c (inc nc))) (recur (rest ds) ns (conj n d))
          :else (recur (rest ds) (conj ns n) [d])))
      (conj ns n))))

(defn select
  "Selection of coordinates where char-at-coord matches predicate."
  [a pred]
  (let [coords (prod (range (rows a)) (range (cols a)))
        selector (fn [coord] (when (pred (at a coord)) {:rc coord :halo (halo a coord)}))]
    (filter some? (map selector coords))))

(defn part1
  [a]
  (->> (numbers a)              ; all numbers & adjacent symbols
       (filter #(seq (:adj %))) ; drop if adjacent to no symbols
       (map :n)                 ; extract numbers
       (apply +)))              ; sum

(let [input (s/split (slurp "03.txt") #"\n")
      a (to-array-2d input)]
  (println (part1 a)))

#_(part1 (to-array-2d ["467..114.."
                     "...*......"
                     "..35..633."
                     "......#..."
                     "617*......"
                     ".....+.58."
                     "..592....."
                     "......755."
                     "...$.*...."
                     ".664.598.."])) ; 4361

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

#_(defn neighbors
[a r c]
(map (fn [[rd cd]] (at a (+ r rd) (+ c cd)))
     (prod [-1 0 +1] [-1 0 +1])))

