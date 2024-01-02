(ns advent.2023.05
  (:require [clojure.string :as s]))

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

(defn overlap?
  [range1 range2]
  (not (or (< (:ub range1) (:lb range2)) (> (:lb range1) (:ub range2)))))

(defn update-range
  [r adj]
  (let [lb1 (:lb r) ub1 (:ub r) lb2 (:lb adj) ub2 (:ub adj) d (:d adj)]
    (if (overlap? r adj)
      (cond
        (and (<= lb2 lb1) (<= lb1 ub2) (< ub2 ub1))
        {:old [{:lb (inc ub2) :ub ub1}] :new [{:lb (+ d lb1) :ub (+ d ub2)}]}
        (< lb1 lb2 ub2 ub1)
        {:old [{:lb lb1 :ub (dec lb2)} {:lb (inc ub2) :ub ub1}] :new [{:lb (+ d lb2) :ub (+ d ub2)}]}
        (and (< lb1 lb2) (<= lb2 ub1) (<= ub1 ub2))
        {:old [{:lb lb1 :ub (dec lb2)}] :new [{:lb (+ d lb2) :ub (+ d ub1)}]}
        :else
        {:old [] :new [{:lb (+ d lb1) :ub (+ d ub1)}]})
      {:old [r] :new []})))

(defn adjust-one
  [ranges adj]
  (loop [ranges ranges old [] new []]
    (if (seq ranges)
      (let [{o :old n :new} (update-range (first ranges) adj)]
        (recur (rest ranges) (apply conj old o) (apply conj new n)))
      {:old old :new new})))

(defn adjust-all
  [maps ranges x]
  (loop [adjs ((maps x) :ranges) old ranges new []]
    (if (seq adjs)
      (let [{o :old n :new} (adjust-one old (first adjs))]
        (recur (rest adjs) o (apply conj new n)))
      {:old old :new new})))

(defn part2
  [categories seeds]
  (let [maps (apply merge (map category->map categories))
        ranges (map (fn [[start n]] {:lb start :ub (- (+ start n) 1)}) (partition 2 seeds))
        locations (loop [ranges ranges x :seed]
                    (if (not= x :location)
                      (let [{o :old n :new} (adjust-all maps ranges x)]
                        (recur (apply conj o n) ((maps x) :to)))
                      ranges))]
    (apply min (map :lb locations))))

(defn go
  []
  (let [blocks (s/split #_almanac (slurp "05.txt") #"(?s)\n\n")
        seeds (strs->nums (last (s/split (first blocks) #": ")))
        categories (rest blocks)]
    (println (part1 categories seeds) (part2 categories seeds))))

(go)
