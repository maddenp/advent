(require '[clojure.string :as s])

(defn h
  [s]
  (reduce #(mod (* 17 (+ %1 %2)) 256) 0 (map int s)))

(defn focusing-power
  [boxes]
  (for [[i lenses] boxes]
    (map-indexed #(* (inc i) (inc %1) (Integer/parseInt (last %2))) lenses)))

(defn lens-ins
  [boxes label fl]
  (let [i (h label) lenses (boxes i)]
    (if (some #(= (first %) label) lenses)
      (assoc boxes i (vec (for [lens lenses] (if (= (first lens) label) [label fl] lens))))
      (assoc boxes i (vec (conj lenses [label fl]))))))

(defn lens-rem
  [boxes label]
  (let [i (h label)]
    (assoc boxes i (vec (remove #(= (first %) label) (boxes i))))))

(defn part1
  [steps]
  (apply + (map h steps)))

(defn part2
  [steps]
  (let [boxes (reduce merge (mapv #(hash-map % []) (range 256)))]
    (loop [steps steps boxes boxes]
      (if (seq steps)
        (let [[label fl] (s/split (first steps) #"[-=]")]
          (recur (rest steps) (if fl (lens-ins boxes label fl) (lens-rem boxes label))))
        (apply + (flatten (focusing-power boxes)))))))

(let [steps (s/split (s/trim-newline (slurp "15.txt")) #",")]
  (println (part1 steps) (part2 steps)))
