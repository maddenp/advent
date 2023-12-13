(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.string :as s])

(def demo (s/join "\n" ["7-F7-"
                        ".FJ|7"
                        "SJLL7"
                        "|F--J"
                        "LJ.LJ"]))

(defn at [a [r c]] (try (aget a r c) (catch Exception _ \.)))
(defn cols [a] (alength (aget a 0)))
(defn rows [a] (alength a))

(defn char->pipe
  [c]
  (or (get {\| \║ \- \═ \L \╚ \J \╝ \7 \╗ \F \╔} c) c))

(defn connects?
  [c dir con]
  (let [fittings {\║ {:n #{\║ \╗ \╔} :e #{        } :s #{\║ \╚ \╝} :w #{        }}
                  \═ {:n #{        } :e #{\═ \╝ \╗} :s #{        } :w #{\═ \╚ \╔}}
                  \╚ {:n #{\║ \╗ \╔} :e #{\═ \╝ \╗} :s #{        } :w #{        }}
                  \╝ {:n #{\║ \╗ \╔} :e #{        } :s #{        } :w #{\═ \╚ \╔}}
                  \╗ {:n #{        } :e #{        } :s #{\║ \╚ \╝} :w #{\═ \╚ \╔}}
                  \╔ {:n #{        } :e #{\═ \╝ \╗} :s #{\║ \╚ \╝} :w #{        }}}]
    (((fittings c) dir) con)))

(def dirs [:n :e :s :w])
(def offsets (zipmap dirs [[-1 0] [0 +1] [+1 0] [0 -1]]))

(defn neighbors
  [a coords]
  (for [dir dirs]
    (let [offset (offsets dir) [r c] coords]
      (at a [(+ r (get offset 0))
             (+ c (get offset 1))]))))

(defn find-s
  [a]
  (->> (cartesian-product (range (rows a)) (range (cols a)))
       (filter #(= \S (at a %)))
       first))

(defn s->pipe
  [a s]
  (let [ns (neighbors a s)]
    ({[0 0 1 1] \╗
      [0 1 0 1] \═
      [0 1 1 0] \╔
      [1 0 0 1] \╝
      [1 0 1 0] \║
      [1 1 0 0] \╚} (mapv #(if (= \. %) 0 1) ns))))

(defn show
  [a]
  (doseq [row (range (rows a))]
    (doseq [col (range (cols a))]
      (print (aget a row col)))
    (println)))

(defn part1
  [a]
  (show a)
  (let [s (find-s a)]
    (aset a (first s) (last s) (s->pipe a s))
    (show a)
    (for [dir dirs]
      (let [offset (offsets dir)
            coords [(+ (first s) (first offset)) (+ (last s) (last offset))]
            con (at a coords)]
        (println con)
        (when (connects? (at a s) dir con) con)))
    ))

#_(println (apply str (map char->pipe demo #_(slurp "10.txt"))))
(let [input (as-> demo #_(slurp "10.txt") $
                  (apply str (map char->pipe $))
                  (s/split $ #"\n")
                  (to-array-2d $)
                  )]
  (println (part1 input)))

;;     #_(s->pipe a)
;;     (for [dir dirs]
;;       (let [offset (offsets dir)
;;             coords [(+ (first s) (first offset)) (+ (last s) (last offset))]
;;             con (at a coords)]
;;         offset
;;         #_(at a coords)
;;         #_(connects? (at a s) dir con)
;;     (as-> [:n :e :s :w] $
;;       (map #(offsets %) $)
;;       (map #(map (fn [[a b]] (+ a b)) (partition 2 (interleave s %))) $)
;;       (map #(at a %) $)
;; ))
         
;;     (map #(at a %) (halo a s))))

; \║ \═ \╚ \╝ \╗ \╔

#_(defn halo
  [a [r c]]
  (->> [[-1 0] [0 +1] [+1 0] [0 -1]]
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter (fn [[hr hc]] (and (not= [hr hc] [r c])
                                  (<= 0 hr (dec (rows a)))
                                  (<= 0 hc (dec (cols a))))))))
