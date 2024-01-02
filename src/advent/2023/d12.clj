(ns advent.2023.d12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def f
  (memoize
    (fn [run springs groups]
      (let [fs (first springs) rs (rest springs) fg (first groups) rg (rest groups)
            hash #(if fg (f true rs (apply conj [(dec fg)] rg)) 0)
            dot #(if run 0 (f false rs groups))]
        (case springs
          [] (case groups ([] [0]) 1 0)
          (case fg
            0 (case fs (\. \?) (f false rs rg) 0)
            (case fs \# (hash) \. (dot) \? (+ (hash) (dot)))))))))

(defn springs-groups
  [record]
  (let [[a b] (s/split record #"\s")]
    [(vec a) (map #(Long/parseLong %) (s/split b #","))]))

(defn unfold
  [record]
  (let [[springs groups] (springs-groups record)]
    [(flatten (interpose \? (repeat 5 springs)))
     (flatten (repeat 5 groups))]))

(defn part1
  [records]
  (apply + (map #(apply f false (springs-groups %)) records)))

(defn part2
  [records]
  (apply + (map #(apply f false (unfold %)) records)))

(defn go
  [& args]
  (let [records (s/split (slurp (io/resource "resources/2023/d12.txt")) #"\n")]
    [(part1 records) (part2 records)]))
