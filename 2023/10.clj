(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.set :refer [intersection union]]
         '[clojure.string :as s])

(def demo (s/join "\n" [".F----7F7F7F7F-7...."
                        ".|F--7||||||||FJ...."
                        ".||.FJ||||||||L7...."
                        "FJL7L7LJLJ||LJ.L-7.."
                        "L--J.L7...LJS7F-7L7."
                        "....F-J..F7FJ|L7L7L7"
                        "....L7.F7||L7|.L7L7|"
                        ".....|FJLJ|FJ|F7|.LJ"
                        "....FJL-7.||.||||..."
                        "....L---J.LJ.LJLJ..."]))

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
  [arr x dist]
  (remove nil?
          (for [dir dirs]
            (let [offset (offsets dir)
                  coords [(+ (first x) (first offset))
                          (+ (last x) (last offset))]]
              (when (and (fits? (at arr x) (at arr coords) dir)
                         (or (not (dist coords)) (> (dist coords) (inc (dist x)))))
                coords)))))

(defn part1
  [arr s]
  (loop [dist {s 0} queue (reduce conj clojure.lang.PersistentQueue/EMPTY [s])]
    (if (empty? queue)
      (apply max (vals dist))
      (let [x (peek queue)
            visit (neighbors-to-visit arr x dist)]
        (recur (merge dist (zipmap visit (repeat (inc (dist x)))))
               (apply conj (pop queue) visit))))))

(defn show
    [arr]
    (doseq [row (range (rows arr))]
      (doseq [col (range (cols arr))]
        (print (aget arr row col)))
      (println)))

#_(defn part2
  [arr s]
  (let [s (s->coords arr)]
    (aset arr (first s) (last s) (s->pipe arr s))
    (show arr)))

(let [arr (as-> #_demo (slurp "10.txt") $
                (apply str (map char->pipe $))
                (s/split $ #"\n")
                (to-array-2d $))
      s (s->coords arr)]
  (aset arr (first s) (last s) (s->pipe arr s))
  (println (part1 arr s) #_(part2 arr s)))
