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

(defn strs->ints
  [strs]
   (map #(Long/parseLong %) (s/split strs #"\s+")))

(defn strs->range
  [strs]
  (let [[dst src n] (strs->ints strs)
        f #(range % (+ % n))]
    (zipmap (f src) (f dst))))

(defn block->map [block]
  (let [lines (s/split block #"\n")
        [_ src dst] (re-matches #"^([^-]+)-to-([^\s]+).*$" (first lines))
        ranges (map strs->range (rest lines))]
    {(keyword src) {:to (keyword dst) :corr (apply merge ranges)}}))

(defn path
  [maps key n]
  (let [map (key maps)]
    (if (= :location key)
      n
      (let [next-key (:to map)
            next-n (get (:corr map) n n)]
        (path maps next-key next-n)))))

(defn part1
  [almanac]
  (let [blocks (s/split almanac #"(?s)\n\n")
        seeds (strs->ints (last (s/split (first blocks) #": ")))
        maps (into {} (map block->map (rest blocks)))]
    (apply min (for [seed seeds] (path maps :seed seed)))))

(println (part1 almanac))
#_(let [almanac (slurp "05.txt")]
  (println (part1 almanac)))
