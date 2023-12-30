(require '[clojure.string :as s])

(declare tilt-e tilt-n tilt-s tilt-w transpose)

(defn spin-cycle
  [lines]
  (->> lines tilt-n tilt-w tilt-s tilt-e))

(defn swap
  [s a b]
  (if (= a b)
    s
    (let [[a b] (sort [a b])]
      (str (subs s 0 a) (get s b) (subs s (inc a) b) (get s a) (subs s (inc b))))))

(defn tilt
  [line]
  (let [n (count line)]
    (loop [i 0 x 0 s line]
      (if (< i n)
        (let [j (inc i)]
          (case (get line i)
            \O (recur j (inc x) (swap s i x))
            \# (recur j j s)
            (recur j x s)))
        s))))

(defn tilt-e
  [lines]
  (let [revstr #(apply str (reverse %))]
    (->> lines (map revstr) (map tilt) (map revstr))))

(defn tilt-n
  [lines]
  (->> lines transpose (map tilt) transpose))

(defn tilt-s
  [lines]
  (->> lines reverse transpose (map tilt) transpose reverse))

(defn tilt-w
  [lines]
  (map tilt lines))

(defn transpose
  [lines]
  (->> lines
       (map #(seq (char-array %)))
       (apply mapv vector)
       (map #(apply str %))))

(defn weigh
  [line]
  (apply + (map-indexed #(* (- (count line) %1) (if (= %2 \O) 1 0)) line)))

(defn part1
  [input]
  (apply + (map weigh (transpose (tilt-n (s/split input #"\n"))))))

(defn part2
  [input]
  (loop [lines (s/split input #"\n") i 0 i2l {} l2i {}]
    (let [next (spin-cycle lines)]
      (if-let [i0 (l2i next)]
        (apply + (map weigh (transpose (i2l (+ i0 (dec (mod (- 1000000000 i0) (- i i0))))))))
        (recur next (inc i) (assoc i2l i next) (assoc l2i next i))))))

(let [input (slurp "14.txt")]
  (println (part1 input) (part2 input)))
