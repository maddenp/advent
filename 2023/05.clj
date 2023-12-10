(require '[clojure.string :as s])

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
    {:start src :end (- (+ src n) 1) :incr (- dst src)}))

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

(defn part2
  [categories seeds]
  (let [ranges (map (fn [[start n]] [start (- (+ start n) 1)]) (partition 2 seeds))]
    (doseq [category categories]
      (pprint (category->map category)))))

(let [blocks (s/split #_almanac (slurp "05.txt") #"(?s)\n\n")
      seeds (strs->nums (last (s/split (first blocks) #": ")))
      categories (rest blocks)]
  (println (part1 categories seeds) #_(part2 categories seeds)))

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
