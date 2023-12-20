(require '[clojure.string :as s])

(def input (s/join "\n" ["???.### 1,1,3"
                         ".??..??...?##. 1,1,3"
                         "?#?#?#?#?#?#?#? 1,3,1,6"
                         "????.#...#... 4,1,1"
                         "????.######..#####. 1,6,5"
                         "?###???????? 3,2,1"]))

(defn f
  [springs groups]
  (println "@@@" springs groups)
  (read-line)
  (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)]
    (if (empty? groups)
      (do (println "groups empty, returning 1") 1)
      (if (empty? springs)
        (do (println "springs empty, returning 0") 0)
        (if (= fg 0)
          (if (or (= fs \.) (= fs \?))
            (do (println "need ." fs "ok") (f rs rg))
            (do (println "need ." fs "bad returning 0") 0))
          (cond (= fs \?)
                (+ (do (println "0 branch" springs groups) (f rs groups))
                   (do (println "1 branch" springs groups) (f rs (apply conj [(dec fg)] rg))))
                (= fs \.)
                (do (println "skipping .") (f rs groups))
                (= fs \#)
                (do (println "matching #") (f rs (apply conj [(dec fg)] rg)))
                ))))))
  
  (defn one
    [record]
    (let [[a b] (s/split record #"\s")
          springs (vec a)
          groups (map #(Long/parseLong %) (s/split b #","))]
      (f springs groups)))

  (let [records (s/split input #"\n")]
    (println (one (first records))))
