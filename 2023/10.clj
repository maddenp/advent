(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.set :refer [intersection union]]
         '[clojure.string :as s])

(def fittings
  {\║ {:n #{\║ \╗ \╔} :e #{        } :s #{\║ \╚ \╝} :w #{        }}
   \═ {:n #{        } :e #{\═ \╝ \╗} :s #{        } :w #{\═ \╚ \╔}}
   \╚ {:n #{\║ \╗ \╔} :e #{\═ \╝ \╗} :s #{        } :w #{        }}
   \╝ {:n #{\║ \╗ \╔} :e #{        } :s #{        } :w #{\═ \╚ \╔}}
   \╗ {:n #{        } :e #{        } :s #{\║ \╚ \╝} :w #{\═ \╚ \╔}}
   \╔ {:n #{        } :e #{\═ \╝ \╗} :s #{\║ \╚ \╝} :w #{        }}})

(def dirs [:n :e :s :w])
(def offsets (zipmap dirs [[-1 0] [0 +1] [+1 0] [0 -1]]))
(def pipes [\║ \═ \╚ \╝ \╗ \╔])

(declare cols coords->neighbors direction rows vertexes)

(defn at [arr [r c]] (try (aget arr r c) (catch Exception _ \.)))
(defn cells [arr] (cartesian-product (range (rows arr)) (range (cols arr))))
(defn char->pipe [c] (or (get (zipmap [\| \- \L \J \7 \F] pipes) c) c))
(defn cols [arr] (alength (aget arr 0)))
(defn det [[r1 c1] [r2 c2]] (- (* r1 c2) (* r2 c1)))
(defn fits? [c con dir] (((fittings c) dir) con))
(defn rows [arr] (alength arr))

(defn clockwise-all
  [arr dists]
  (let [circuit (set (keys dists))
        s (first (sort-by second (set (vertexes arr circuit))))]
    (loop [x s visited #{} cw []]
      (if (= (count visited) (count circuit))
        cw
        (let [circuit-neighbors (filter #(circuit %) (coords->neighbors x))
              next-x (first
                       (filter
                         #(and (not (visited %))
                               (seq ((fittings (at arr x)) (direction x %))))
                         circuit-neighbors))]
          (recur next-x (conj visited x) (conj cw x)))))))

(defn clockwise-vertexes
  [arr dists]
  (vertexes arr (clockwise-all arr dists)))

(defn coords->neighbors
  [coords]
  (for [dir dirs]
    (let [offset (offsets dir) [r c] coords]
      [(+ r (get offset 0)) (+ c (get offset 1))])))

(defn coords->neighbors-at
  [arr coords]
  (map (partial at arr) (coords->neighbors coords)))

(defn direction
  [[r1 c1] [r2 c2]]
  (cond
    (> r1 r2) :n
    (> c2 c1) :e
    (> r2 r1) :s
    (> c1 c2) :w))

(defn neighbors-to-visit
  [arr x dists]
  (remove nil?
          (for [dir dirs]
            (let [offset (offsets dir)
                  coords [(+ (first x) (first offset))
                          (+ (last x) (last offset))]]
              (when (and (fits? (at arr x) (at arr coords) dir)
                         (or (not (dists coords)) (> (dists coords) (inc (dists x)))))
                coords)))))

(defn plausible-neighbors
  [pipe neighbors]
  (apply union (map #(intersection ((fittings pipe) %) (set [(neighbors %)])) dirs)))

(defn s->coords
  [arr]
  (->> (cells arr)
       (filter #(= \S (at arr %)))
       first
       (apply vector)))

(defn s->pipe
  [arr s]
  (let [neighbors (zipmap dirs (coords->neighbors-at arr s))]
    (->> pipes
         (map #(vector % (plausible-neighbors % neighbors)))
         (filter #(= 2 (count (last %))))
         first
         first)))

(defn score-cells
  [arr s]
  (loop [dists {s 0} queue (reduce conj clojure.lang.PersistentQueue/EMPTY [s])]
    (if (empty? queue)
      dists
      (let [x (peek queue)
            visit (neighbors-to-visit arr x dists)]
        (recur (merge dists (zipmap visit (repeat (inc (dists x)))))
               (apply conj (pop queue) visit))))))

(defn vertexes
  [arr circuit]
  (filter #(#{\╚ \╝ \╗ \╔} (at arr %)) circuit))

(defn part1
  [dists]
  (apply max (vals dists)))

(defn part2
  [arr dists]
  ;; Uses the Shoelace Formula and Pick's Theorem.
  (let [b (count (clockwise-all arr dists))
        cwv (vec (clockwise-vertexes arr dists))
        A (as-> (first cwv) $
            (conj cwv $)
            (map vector $ (rest $))
            (map #(det (first %) (last %)) $)
            (apply + $)
            (abs $)
            (/ $ 2))]
    (+ (- A (/ b 2)) 1)))

(let [arr (as-> (slurp "10.txt") $
            (apply str (map char->pipe $))
            (s/split $ #"\n")
            (to-array-2d $))
      s (s->coords arr)]
  (aset arr (first s) (last s) (s->pipe arr s))
  (let [dists (score-cells arr s)]
    (println (part1 dists) (part2 arr dists))))

#_(defn show
  [arr]
  (doseq [row (range (rows arr))]
    (doseq [col (range (cols arr))]
      (print (aget arr row col)))
    (println)))
