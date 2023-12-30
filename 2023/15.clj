(require '[clojure.string :as s])

(def input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn h
  [s]
  (reduce #(mod (* 17 (+ %1 %2)) 256) 0 (map int s)))

(defn part1
  [steps]
  (apply + (map h steps)))

(let [steps (s/split (s/trim-newline #_input (slurp "15.txt")) #",")]
  (println (part1 steps) #_(part2 steps)))
