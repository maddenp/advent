(ns advent.common)

(defn at [arr [r c]] (try (aget arr r c) (catch Exception _ \.)))
(defn cols [arr] (alength (aget arr 0)))
(defn rows [arr] (alength arr))

(defn show
  [arr]
  (doseq [row (range (rows arr))]
    (doseq [col (range (cols arr))]
      (print (aget arr row col)))
    (println)))
