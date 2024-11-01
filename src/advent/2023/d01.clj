(ns advent.2023.d01
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn mark-one
  [word]
  (let [numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]]
    (->> (for [[k v] (zipmap numbers (range (count numbers)))]
           (when (s/starts-with? word k)
             (apply str v (subs word 1))))
         (filter some?)
         first)))

(defn mark-all
  [word]
  (loop [word word marked ""]
    (if (< (count word) 1)
      marked
      (let [w (or (mark-one word) word)]
        (recur (subs w 1) (str marked (first w)))))))

(defn part1
  [text]
  (let [digit? #(<= (int \0) (int %) (int \9))
        f #(Integer/parseInt (let [d (filter digit? %)] (str (first d) (last d))))]
    (apply + (map f (s/split text #"\n")))))

(defn part2
  [text]
  (part1 (s/join "\n" (map mark-all (s/split text #"\n")))))

(defn go
  [& _]
  (let [input (slurp (io/resource "resources/2023/d01.txt"))]
    [(part1 input) (part2 input)]))

(defn -main
  [& _]
  (apply println (go)))
