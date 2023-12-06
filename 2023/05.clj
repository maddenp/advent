(require '[clojure.string :as s])

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
           "56 93 4"]))

(defn strs->nums
  [strs]
   (map #(Long/parseLong %) (s/split strs #"\s+")))

(defn strs->incl
  [strs]
  (let [[dst src n] (strs->nums strs)]
    #(when (<= src % (dec (+ src n))) (+ dst (- % src)))))

(defn block->map [block]
  (let [lines (s/split block #"\n")
        [_ cat1 cat2] (re-matches #"^([^-]+)-to-([^\s]+).*$" (first lines))
        incls (map strs->incl (rest lines))
        f (fn [s] (first (filter identity (conj (mapv #(% s) incls) s))))] ; PM simplify
    {(keyword cat1) {:to (keyword cat2) :corr f}}))

(defn path
  [maps key n]
  (if (= :location key)
    n
    (let [map (key maps)]
      (path maps (:to map) ((:corr map) n)))))

(defn part1
  [almanac]
  (let [blocks (s/split almanac #"(?s)\n\n")
        seeds (strs->nums (last (s/split (first blocks) #": ")))
        maps (into {} (map block->map (rest blocks)))]
    (apply min (for [seed seeds] (path maps :seed seed)))))

(let [almanac (slurp "05.txt")]
  (println (part1 almanac)))
