(ns day-18.core
  (:gen-class))

(defn neighbors->tile
  [nbrs]
  (condp = nbrs
    [\^ \^ \.] \^
    [\. \^ \^] \^
    [\^ \. \.] \^
    [\. \. \^] \^
    \.))

(defn next-row
  [s]
  (->>
    s
    (apply list \.)
    (partition 3 1 [\.])
    (map neighbors->tile)
    (apply str)))

(defn next-rows
  [r n]
  (->>
    (iterate next-row r)
    (drop 1)
    (take n)
    vec))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
