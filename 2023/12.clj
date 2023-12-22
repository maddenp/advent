(require '[clojure.string :as s])

(def input (s/join "\n" ["???.### 1,1,3"
                         ".??..??...?##. 1,1,3"
                         "?#?#?#?#?#?#?#? 1,3,1,6"
                         "????.#...#... 4,1,1"
                         "????.######..#####. 1,6,5"
                         "?###???????? 3,2,1"]))

(defn f
  [springs groups run line]
  (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)
        damaged #(f rs (apply conj [(dec fg)] rg) true (str line "#"))
        operational #(f rs groups false (str line "."))]
    (if (empty? springs)
      (if (or (nil? fg) (= groups [0])) 1 0)
      (if (= fg 0)
        (if (or (= fs \.) (= fs \?)) (f rs rg false (str line ".")) 0)
        (cond (= fs \#)
              (if fg (damaged) 0)
              (= fs \.)
              (if run 0 (operational))(= fs \?)
              (+ (if run 0 (operational))
                 (if fg (damaged) 0)))))))

(defn one
  [record]
  (let [[a b] (s/split record #"\s")
        springs (vec a)
        groups (map #(Long/parseLong %) (s/split b #","))]
    (f springs groups false "")))

(let [records (s/split #_input (slurp "12.txt") #"\n")]
  (println (apply + (map one records))))
