(require '[clojure.string :as s])

(def input (s/join "\n" ["???.### 1,1,3"
                         ".??..??...?##. 1,1,3"
                         "?#?#?#?#?#?#?#? 1,3,1,6"
                         "????.#...#... 4,1,1"
                         "????.######..#####. 1,6,5"
                         "?###???????? 3,2,1"]))

(def f
  (memoize
    (fn [springs groups run]
      (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)
            hash #(if fg (f rs (apply conj [(dec fg)] rg) true) 0)
            dot #(if run 0 (f rs groups false))]
        (case springs
          [] (case groups ([] [0]) 1 0)
          (case fg
            0 (case fs (\. \?) (f rs rg false) 0)
            (case fs \# (hash) \. (dot) \? (+ (hash) (dot)))))))))

(defn springs-groups
  [record]
  (let [[a b] (s/split record #"\s")]
    [(vec a) (map #(Long/parseLong %) (s/split b #","))]))

(defn one
  [record]
  (let [[springs groups] (springs-groups record)]
    (f springs groups false)))

(defn part1
  [records]
  (apply + (map one records)))

(defn part2
  [records]
  (first records))

(let [records (s/split #_input (slurp "12.txt") #"\n")]
  (println (part1 records) #_(part2 records)))
