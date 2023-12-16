(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.set :refer [difference intersection union]]
         '[clojure.string :as s])

(def demo0 (s/join "\n" ["..........."
                         ".S-------7."
                         ".|F-----7|."
                         ".||.....||."
                         ".||.....||."
                         ".|L-7.F-J|."
                         ".|..|.|..|."
                         ".L--J.L--J."
                         "..........."]))

(def demo1 (s/join "\n" [".F----7F7F7F7F-7...."
                         ".|F--7||||||||FJ...."
                         ".||.FJ||||||||L7...."
                         "FJL7L7LJLJ||LJ.L-7.."
                         "L--J.L7...LJS7F-7L7."
                         "....F-J..F7FJ|L7L7L7"
                         "....L7.F7||L7|.L7L7|"
                         ".....|FJLJ|FJ|F7|.LJ"
                         "....FJL-7.||.||||..."
                         "....L---J.LJ.LJLJ..."]))

(def demo2 (s/join "\n" ["FF7FSF7F7F7F7F7F---7"
                         "L|LJ||||||||||||F--J"
                         "FL-7LJLJ||||||LJL-77"
                         "F--JF--7||LJLJ7F7FJ-"
                         "L---JF-JLJ.||-FJLJJ7"
                         "|F|F-JF---7F7-L7L|7|"
                         "|FFJF7L7F-JF7|JL---7"
                         "7-L-JL7||F7|L7F-7F7|"
                         "L.L7LFJ|||||FJL7||LJ"
                         "L7JLJL-JLJLJL--JLJ.L"]))

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
(defn cells [arr] (cartesian-product (range (rows arr)) (range (cols arr))))

(defn coords->neighbors
  [coords]
  (for [dir dirs]
    (let [offset (offsets dir) [r c] coords]
      [(+ r (get offset 0)) (+ c (get offset 1))])))

(defn coords->neighor-at
  [arr coords]
  (map (partial at arr) (coords->neighbors coords)))

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
  (let [neighbors (zipmap dirs (coords->neighor-at arr s))]
    (->> pipes
         (map #(vector % (plausible-neighbors % neighbors)))
         (filter #(= 2 (count (last %))))
         first
         first)))

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

(defn show
  [arr]
  (doseq [row (range (rows arr))]
    (doseq [col (range (cols arr))]
      (print (aget arr row col)))
    (println)))

(defn score-cells
  [arr s]
  (loop [dists {s 0} queue (reduce conj clojure.lang.PersistentQueue/EMPTY [s])]
    (if (empty? queue)
      dists
      (let [x (peek queue)
            visit (neighbors-to-visit arr x dists)]
        (recur (merge dists (zipmap visit (repeat (inc (dists x)))))
               (apply conj (pop queue) visit))))))

(defn part1
  [dists]
  (apply max (vals dists)))

#_(defn coords->all-neighbors
  [arr coords]
  (let [[r c] coords
        nr (rows arr)
        nc (cols arr)]
    (->> (apply cartesian-product (repeat 2 [-1 0 +1]))
         (map #(vector (+ r (first %)) (+ c (last %))))
         (filter #(and (not= % coords) (< -1 (first %) nr) (< -1 (last %) nc))))))

#_(defn outside?
  [arr circuit coords]
  #_(println "@@@" coords)
  (let [[r c] coords
        neighbors (coords->all-neighbors arr coords)
        non-circuit (difference (set neighbors) circuit)]
    (println "+++" neighbors non-circuit)
    (read-line)
    (if (or (zero? r) (zero? c))
      true
      (some (partial outside? arr circuit) non-circuit))))
  
#_(defn part2
  [arr dists]
  (show arr)
  (let [circuit (set (keys dists))
        candidates (difference (set (cells arr)) circuit)]
    (outside? arr circuit [2 2])))

#_(def possible-directions
  (apply hash-map
         (interleave pipes [#{:n :s}
                            #{:e :w}
                            #{:n :e}
                            #{:n :w}
                            #{:s :w}
                            #{:e :s}])))

(defn direction
  [[r1 c1] [r2 c2]]
  (cond
    (> r1 r2) :n
    (> c2 c1) :e
    (> r2 r1) :s
    (> c1 c2) :w))

(defn vertexes
  [arr circuit]
  (filter #(#{\╚ \╝ \╗ \╔} (at arr %)) circuit))

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

(defn det
  [[r1 c1] [r2 c2]]
  (- (* r1 c2) (* r2 c1)))

(defn part2-shoelace
  [arr dists]
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

; \║ \═ \╚ \╝ \╗ \╔

;; (defn look
;;   [arr curr-coord next-coord]
;;   (let [n [-1 0] e [0 +1] s [+1 0] w [0 -1] x [0 0]
;;         cc (at arr curr-coord) nc (at arr next-coord)
;;         d (direction curr-coord next-coord)
;;         offset (cond
;;                  (= cc \║) ({:n [] :s [w]} d)
;;                  (= cc \═) ({:e [s] :w []} d)
;;                  (= cc \╚) ({:n [] :e [s w]} d)
;;                  (= cc \╝) ({:n [e s] :w []} d)
;;                  (= cc \╗) ({:s [x] :w [n e]} d)
;;                  (= cc \╔) ({:s [] :e [x]} d)

;; (defn look
;;   [arr coord last]
;;   (let [c (at arr coord)]
;;     (if (nil? pointing)
;;       ({\║ [:e] \╚ [:n :e] \╔ [:e :s]} c)
;;       (cond
;;         (= c \║) ({[:s :w] [:w   ] [:w   ] [:w   ]} last)
;;         (= c \═) ({[:e :s] [:s   ] [:s :w] [:s   ]} last)
;;         (= c \╚) ({[:s :w] [:s :w] [:w   ] [:s :w]} last)
;;         (= c \╝) ({[:s   ] [:e :s] [:s :w] [:e :s]} last)
;;         (= c \╗) ({[:e :s] [:s :w] [:s   ] [:s :w]} last)
;;         (= c \╔) ({[:e :s] [:e :s]} last)

(defn inside-coord
  #_[[curr-coord next-coord]]
  [arr [curr-coord next-coord]]
  (let [[r c] curr-coord
        d (direction curr-coord next-coord)
        offsets {:n [0 +1] :e [+1 0] :s [0 -1] :w [-1 0]}
        offset (offsets d)]
    (println "@@@" "r" r "c" c "curr" curr-coord (at arr curr-coord) "next" next-coord (at arr next-coord) "direction" d "offset" offset)
    [(+ r (offset 0)) (+ c (offset 1))]))

(defn inside-coords-circuit-adjacent
  [arr dists]
  (let [circuit (set (keys dists))
        cw (clockwise-all arr dists)]
    (->> (map vector cw (rest cw))
         #_(map inside-coord)
         (map (partial inside-coord arr))
         (remove circuit)
         (remove nil?)
         set)))

(defn inside-all
  [arr dists]
  (let [circuit (set (keys dists))
        icca (inside-coords-circuit-adjacent arr dists)]
    (loop [check icca inside icca]
      (if (seq check)
        (let [new (filter #(not (or (circuit %) (inside %))) (coords->neighbors (first check)))]
          (recur (apply conj (rest check) new) (apply conj inside new)))
        inside))))

(defn part2
  [arr dists]
  (show arr)
  #_(count (inside-all arr dists)))

#_(defn part2
  [arr dists]
  (let [circuit (set (keys dists))]
    (doseq [[r c] (cartesian-product (range (rows arr)) (range (cols arr)))]
      (when (not (circuit [r c]))
        (aset arr r c \.)))
    #_(show arr)
    (doseq [[r c] (inside-all arr dists)]
      (aset arr r c \*))
    (show arr)))

(let [arr (as-> #_demo2 (slurp "10.txt") $
                (apply str (map char->pipe $))
                (s/split $ #"\n")
                (to-array-2d $))
      s (s->coords arr)]
  (aset arr (first s) (last s) (s->pipe arr s))
  (let [dists (score-cells arr s)]
    (println (part1 dists) (part2-shoelace arr dists))))
