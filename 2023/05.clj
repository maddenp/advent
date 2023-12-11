(require '[clojure.math.combinatorics :refer [cartesian-product]]
         '[clojure.string :as s])

(defn strs->nums
  [strs]
  (map #(Long/parseLong %) (s/split strs #"\s+")))

(defn strs->incl
  [strs]
  (let [[dst src n] (strs->nums strs)]
    #(when (<= src % (dec (+ src n))) (+ dst (- % src)))))

(defn strs->range
  [strs]
  (let [[dst src n] (strs->nums strs)]
    {:lb src :ub (- (+ src n) 1) :d (- dst src)}))

(defn category->map [category]
  (let [lines (s/split category #"\n")
        [_ cat1 cat2] (re-matches #"^([^-]+)-to-([^\s]+).*$" (first lines))
        incls (map strs->incl (rest lines))
        next (fn [s] (first (filter identity (conj (mapv #(% s) incls) s))))]
    {(keyword cat1) {:to (keyword cat2) :next next :ranges (map strs->range (rest lines))}}))

(defn path
  [maps key n]
  (let [map (key maps)]
    (if (= :location key) n (recur maps (:to map) ((:next map) n)))))

(defn part1
  [categories seeds]
  (let [maps (into {} (map category->map categories))]
    (reduce min (for [seed seeds] (path maps :seed seed)))))

(require '[clojure.pprint :refer [pprint]])

(defn overlap?
  [range1 range2]
  (not (or (< (:ub range1) (:lb range2)) (> (:lb range1) (:ub range2)))))

(defn update
  [range1 range2]
  (println ">>> inp" range1 "vs" range2)
  (let [lb1 (:lb range1) ub1 (:ub range1) lb2 (:lb range2) ub2 (:ub range2) d (:d range2)]
    (let [res
    (if (overlap? range1 range2)
      (cond
        (and (<= lb2 lb1) (< lb1 ub2 ub1))
        {:old [{:lb (inc ub2) :ub ub1}] :new [{:lb (+ d lb1) :ub (+ d ub2)}]}
        (and (> lb2 lb1) (< ub2 ub1))
        {:old [{:lb lb1 :ub (dec lb2)} {:lb (inc ub2) :ub ub1}] :new [{:lb (+ d lb2) :ub (+ d ub2)}]}
        (and (< lb1 lb2 ub1) (>= ub2 ub1))
        {:old [{:lb lb1 :ub (dec lb2)}] :new [{:lb (+ d lb2) :ub (+ d ub1)}]}
        :else
        {:old [] :new [{:lb (+ d lb1) :ub (+ d ub1)}]})
      {:old [range1] :new []})
          ] (println ">>> out" res) res)
          ))
    
(defn part2
  [categories seeds]
  (let [maps (apply merge (map category->map categories))
        f (fn [[start n]] {:lb start :ub (- (+ start n) 1)})
        ranges (map f (partition 2 seeds))]
    (loop [ranges ranges x :seed]
      (println "@@@" ranges)
      (if (= x :soil) ; :location
        888
        (do
          (let [final (loop [adj ((maps x) :ranges) adj-old [] adj-new []]
                        (println "--- adj" (first adj))
                        (if (seq adj)
                          (loop [r ranges old [] new []]
                            (println "+++ start" "r" r "old" old "new" new) 
                            (if (seq r)
                              (let [res (update (first r) (first adj))]
                                (println "+++ res old" (:old res) "res new" (:new res))
                                (recur (rest r) (flatten (conj old (:old res))) (flatten (conj new new (:new res)))))
                              new))
                          (recur (rest adj) adj-old adj-new)))]
            (println "===" final))
          (recur ranges ((maps x) :to)))
        ))))

(let [blocks (s/split almanac #_(slurp "05.txt") #"(?s)\n\n")
      seeds (strs->nums (last (s/split (first blocks) #": ")))
      categories (rest blocks)]
  (println #_(part1 categories seeds) (part2 categories seeds)))

(comment
  (def almanac
    (s/join "\n"
            ["seeds: 79 14 55 13"
             ""
             "seed-to-soil map:"
             "50 98 2"
             "52 50 48"
             ""
             "soil-to-fertilizer map:"
             "0 15 37"
             "37 52 2"
             "39 0 15"
             ""
             "fertilizer-to-water map:"
             "49 53 8"
             "0 11 42"
             "42 0 7"
             "57 7 4"
             ""
             "water-to-light map:"
             "88 18 7"
             "18 25 70"
             ""
             "light-to-temperature map:"
             "45 77 23"
             "81 45 19"
             "68 64 13"
             ""
             "temperature-to-humidity map:"
             "0 69 1"
             "1 0 69"
             ""
             "humidity-to-location map:"
             "60 56 37"
             "56 93 4"])))
