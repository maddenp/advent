(require '[clojure.string :as s])

(def input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn h
  [s]
  (reduce #(mod (* 17 (+ %1 %2)) 256) 0 (map int s)))

(defn focusing-power
  [boxes]
  (remove #(empty? (last %)) boxes))

(defn lens-ins
  [boxes label fl]
  (let [i (h label) lenses (boxes i)]
    (if (some #(= (first %) label) lenses)
      (assoc boxes i (for [lens lenses] (if (= (first lens) label) [label fl] lens)))
      (assoc boxes i (conj lenses [label fl])))))

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
          (if fl
            (recur (rest steps) (lens-ins boxes label fl))
            (recur (rest steps) (lens-rem boxes label))))
        (focusing-power boxes)))))

(require '[clojure.pprint :refer [pprint]])

(let [steps (s/split (s/trim-newline input #_(slurp "15.txt")) #",")]
  (pprint #_(part1 steps) (part2 steps)))
