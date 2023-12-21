(require '[clojure.string :as s])

(def input (s/join "\n" ["???.### 1,1,3"
                         ".??..??...?##. 1,1,3"
                         "?#?#?#?#?#?#?#? 1,3,1,6"
                         "????.#...#... 4,1,1"
                         "????.######..#####. 1,6,5"
                         "?###???????? 3,2,1"]))

(defn f
  [springs groups run line]
  (println "@@@" springs groups (str \" line \"))
  #_(read-line)
  (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)]
    (if (empty? springs)
      (if (or (nil? fg) (= groups [0]))
        (do (println "groups empty, RETURNING 1 FOR" (str \" line \")) 1)
        (do (println "springs empty, returning 0") 0))
      (if (= fg 0)
        (if (or (= fs \.) (= fs \?))
          (do (println "need . " (str "(accepting " fs ")")) (f rs rg false (str line ".")))
          (do (println "need ." fs "bad returning 0") 0))
        (cond (= fs \?)
              (+ (do (println ". branch" springs groups)
                     (if run 0 (f rs groups false (str line "."))))
                 (do (println "# branch" springs groups)
                     (if fg (f rs (apply conj [(dec fg)] rg) true (str line "#")) 0)))
              (= fs \.)
              (do (println "skipping .") (f rs groups false (str line ".")))
              (= fs \#)
              (do (println "matching #") (if fg (f rs (apply conj [(dec fg)] rg) true (str line "#")) 0))
              )))))

(defn one
  [record]
  (let [[a b] (s/split record #"\s")
        springs (vec a)
        groups (map #(Long/parseLong %) (s/split b #","))]
    (f springs groups false "")))

(let [records (s/split #_input (slurp "12.txt") #"\n")]
  (println (apply + (map one records))))
