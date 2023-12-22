(require '[clojure.string :as s])

(def input (s/join "\n" ["???.### 1,1,3"
                         ".??..??...?##. 1,1,3"
                         "?#?#?#?#?#?#?#? 1,3,1,6"
                         "????.#...#... 4,1,1"
                         "????.######..#####. 1,6,5"
                         "?###???????? 3,2,1"]))

(defn f
  [springs groups run]
  (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)
        hash #(if fg (f rs (apply conj [(dec fg)] rg) true) 0)
        dot #(if run 0 (f rs groups false))]
    (if (empty? springs)
      (case groups ([] [0]) 1 0)
      (if (= fg 0)
        (case fs (\. \?) (f rs rg false) 0)
        (case fs \# (hash) \. (dot) \? (+ (hash) (dot)))))))

(defn one
  [record]
  (let [[a b] (s/split record #"\s")
        springs (vec a)
        groups (map #(Long/parseLong %) (s/split b #","))]
    (f springs groups false)))

(let [records (s/split #_input (slurp "12.txt") #"\n")]
  (println (apply + (map one records))))
