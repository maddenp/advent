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

(defn at [a [r c]] (try (aget a r c) (catch Exception _ \.)))
(defn char->pipe [c] (or (get (zipmap [\| \- \L \J \7 \F] pipes) c) c))
(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))
(defn fits? [c con dir] (((fittings c) dir) con))

(defn coords->neighbors
  [a coords]
  (for [dir dirs]
    (let [offset (offsets dir) [r c] coords]
      (at a [(+ r (get offset 0))
             (+ c (get offset 1))]))))

(defn s->coords
  [a]
  (->> (cartesian-product (range (rows a)) (range (cols a)))
       (filter #(= \S (at a %)))
       first))

(defn plausible-neighbors
  [pipe neighbors]
  (apply union (map #(intersection ((fittings pipe) %) (set [(neighbors %)])) dirs)))

(defn s->pipe
  [a s]
  (let [neighbors (zipmap dirs (coords->neighbors a s))]
    (->> pipes
         (map #(vector % (plausible-neighbors % neighbors)))
         (filter #(= 2 (count (last %))))
         first
         first)))

(defn show
  [a]
  (doseq [row (range (rows a))]
    (doseq [col (range (cols a))]
      (print (aget a row col)))
    (println)))

(defn part1
  [a]
  (let [s (s->coords a)]
    (aset a (first s) (last s) (s->pipe a s))
    (loop [d {s 0} q (reduce conj clojure.lang.PersistentQueue/EMPTY [s])]
      (if (empty? q)
        (apply max (vals d))
        (let [x (peek q)
              visit (remove nil?
                            (for [dir dirs]
                              (let [offset (offsets dir)
                                    coords [(+ (first x) (first offset))
                                            (+ (last x) (last offset))]]
                                (when (and (fits? (at a x) (at a coords) dir)
                                           (or (not (d coords)) (> (d coords) (inc (d x)))))
                                  coords))))]
          (recur (merge d (zipmap visit (repeat (inc (d x)))))
                 (apply conj (pop q) visit)))))))

(let [input (as-> #_demo (slurp "10.txt") $
                  (apply str (map char->pipe $))
                  (s/split $ #"\n")
                  (to-array-2d $))]
  (println (part1 input)))
