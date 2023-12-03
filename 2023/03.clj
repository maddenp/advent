(require '[clojure.math.combinatorics :refer [cartesian-product] :rename {cartesian-product prod}]
         '[clojure.set :refer [difference]]
         '[clojure.string :as s])

(declare halo runs)

(defn at [a [r c]] (try (aget a r c) (catch Exception _ \.)))
(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn digits
  "Coordinates containing digits."
  [a]
  (let [coords (prod (range (rows a)) (range (cols a)))
        digit? #(<= (int \0) (int %) (int \9))
        selector (fn [coord] (when (digit? (at a coord)) {:rc coord :halo (halo a coord)}))]
    (filter some? (map selector coords))))

(defn halo
  "All in-bounds adjacent cooridnates."
  [a [r c]]
  (->> (prod [-1 0 +1] [-1 0 +1])
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter (fn [[hr hc]] (and (not= [hr hc] [r c])
                                  (<= 0 hr (dec (rows a)))
                                  (<= 0 hc (dec (cols a))))))))

(defn numbers
  "Numbers and their adjacent coords."
  [a]
  (for [run (runs a)]
    (let [rcs (map :rc run)]
      {:n (->> (map #(at a %) rcs)          ; coords -> chars
               (apply str)                  ; combine chars -> str
               Integer/parseInt)            ; str -> int
       :adj (as-> (map :halo run) $         ; get all adjacent coords
              (reduce into [] $)            ; all coords in one vector
              (set $)                       ; remove duplicates
              (difference $ (set rcs)))}))) ; remove coords belonging to number

(defn runs
  "Groups of runs of successive digits in the same row."
  [a]
  (loop [ds (digits a) ns [] n nil]
    (if (seq ds)
      (let [d (first ds) [r c] (:rc d) [nr nc] (:rc (last n))]
        (cond
          (nil? n) (recur (rest ds) ns [d])
          (and (= r nr) (= c (inc nc))) (recur (rest ds) ns (conj n d))
          :else (recur (rest ds) (conj ns n) [d])))
      (conj ns n))))

(defn part1
  [a]
  (->> (for [n (numbers a)]
         {:n (:n n)
          :adj (->> (:adj n)                 ; all numbers & adjacent coords
                    (map #(at a %))          ; coords -> chars
                    set                      ; remove duplicates
                    (filter #(not= \. %)))}) ; remove '.'s
       (filter #(seq (:adj %)))              ; drop if adjacent to no symbols
       (map :n)                              ; extract numbers
       (apply +)))                           ; sum

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
