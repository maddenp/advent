(require '[clojure.string :as s]
         '[clojure.math.combinatorics :as c])

(require '[clojure.pprint :only pprint])

(def neighbor-offsets (c/cartesian-product [-1 0 +1] [-1 0 +1]))

(defn digit? [c] (<= (int \0) (int c) (int \9)))
(defn symbol? [c] (not (or (digit? c) (= \. c))))

#_(let [input (s/split (slurp "03.txt") #"\n")
        a (to-array-2d input)
        h (alength a)
        w (alength (aget a 0))]
    [h w])

(def a
  (to-array-2d
    ["467..114.."
     "...*......"
     "..35..633."
     "......#..."
     "617*......"
     ".....+.58."
     "..592....."
     "......755."
     "...$.*...."
     ".664.598.."]))
(def h (alength a))
(def w (alength (aget a 0)))

(defn at
  [a r c]
  (try (aget a r c) (catch Exception e \.)))

(defn neighbor-at
  [a r c [rd cd]]
  (at a (+ r rd) (+ c cd)))

(defn neighbors
  [a r c]
  (map #(neighbor-at a r c %) neighbor-offsets))

;; (defn isnum?
;;   [coll]
;;   (some some? coll))

(let [runs (for [[r c] (c/cartesian-product (range h) (range w))]
             (let [x (at a r c)]
               (when (digit? x)
                 {(some symbol? (neighbors a r c)) x})))]
  (->> runs
       (partition-by some?)
       (filter #(some true? (flatten (map keys %))))
       (map #(Integer/parseInt (apply str (flatten (map vals %)))))
       (apply +)
       ))

#_(some true? (flatten (map keys [{nil \4} {nil \6} {true \7}])))
;; (doseq [x
;; (map #(filter isnum? (partition-by some? %))
;;      (for [r (range h)]
;;        (for [c (range w)]
;;          (when (digit? (at a r c))
;;            {(some symbol? (neighbors a r c)) (at a r c)}))))
;;         ]
;;   (pprint x))

#_(doseq [r (range h)]
  (doseq [c (range w)]
    (when (digit? (at a r c))
      (let [neighbors (map #(neighbor-at a r c %) neighbor-offsets)]
        (println (at a r c) neighbors(some symbol? neighbors))))))
