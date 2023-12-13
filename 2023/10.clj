(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.set :refer [intersection union]]
         '[clojure.string :as s])

(def demo (s/join "\n" ["7-F7-"
                        ".FJ|7"
                        "SJLL7"
                        "|F--J"
                        "LJ.LJ"]))

(def dirs [:n :e :s :w])
(def offsets (zipmap dirs [[-1 0] [0 +1] [+1 0] [0 -1]]))
(def pipes [\║ \═ \╚ \╝ \╗ \╔])

(def fittings
  {\║ {:n #{\║ \╗ \╔} :e #{        } :s #{\║ \╚ \╝} :w #{        }}
   \═ {:n #{        } :e #{\═ \╝ \╗} :s #{        } :w #{\═ \╚ \╔}}
   \╚ {:n #{\║ \╗ \╔} :e #{\═ \╝ \╗} :s #{        } :w #{        }}
   \╝ {:n #{\║ \╗ \╔} :e #{        } :s #{        } :w #{\═ \╚ \╔}}
   \╗ {:n #{        } :e #{        } :s #{\║ \╚ \╝} :w #{\═ \╚ \╔}}
   \╔ {:n #{        } :e #{\═ \╝ \╗} :s #{\║ \╚ \╝} :w #{        }}})

(defn at [arr [r c]] (try (aget arr r c) (catch Exception _ \.)))
(defn char->pipe [c] (or (get (zipmap [\| \- \L \J \7 \F] pipes) c) c))
(defn cols [arr] (alength (aget arr 0)))
(defn rows [arr] (alength arr))
(defn fits? [c con dir] (((fittings c) dir) con))

#_(defn show
    [arr]
    (doseq [row (range (rows arr))]
      (doseq [col (range (cols arr))]
        (print (aget arr row col)))
      (println)))

(defn coords->neighbors
  [arr coords]
  (for [dir dirs]
    (let [offset (offsets dir) [r c] coords]
      (at arr [(+ r (get offset 0))
               (+ c (get offset 1))]))))

(defn plausible-neighbors
  [pipe neighbors]
  (apply union (map #(intersection ((fittings pipe) %) (set [(neighbors %)])) dirs)))

(defn s->coords
  [arr]
  (->> (cartesian-product (range (rows arr)) (range (cols arr)))
       (filter #(= \S (at arr %)))
       first))

(defn s->pipe
  [arr s]
  (let [neighbors (zipmap dirs (coords->neighbors arr s))]
    (->> pipes
         (map #(vector % (plausible-neighbors % neighbors)))
         (filter #(= 2 (count (last %))))
         first
         first)))

(defn neighbors-to-visit
  [arr x d]
  (remove nil?
          (for [dir dirs]
            (let [offset (offsets dir)
                  coords [(+ (first x) (first offset))
                          (+ (last x) (last offset))]]
              (when (and (fits? (at arr x) (at arr coords) dir)
                         (or (not (d coords)) (> (d coords) (inc (d x)))))
                coords)))))

(defn part1
  [arr]
  (let [s (s->coords arr)]
    (aset arr (first s) (last s) (s->pipe arr s))
    (loop [d {s 0} q (reduce conj clojure.lang.PersistentQueue/EMPTY [s])]
      (if (empty? q)
        (apply max (vals d))
        (let [x (peek q)
              v (neighbors-to-visit arr x d)]
          (recur (merge d (zipmap v (repeat (inc (d x)))))
                 (apply conj (pop q) v)))))))

(let [input (as-> #_demo (slurp "10.txt") $
                  (apply str (map char->pipe $))
                  (s/split $ #"\n")
                  (to-array-2d $))]
  (println (part1 input)))
