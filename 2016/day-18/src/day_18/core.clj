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

(defn collect-rows
  [n r]
  (->>
    (iterate next-row r)
    (take n)
    vec))

(defn count-row
  [r]
  (->>
    r
    (filter #(= % \.))
    count))

(defn count-safe
  [rs]
  (->>
    rs
    (reduce
      (fn
        [s r]
        (+ s (count-row r))) 0)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (str
      ".^^^^^.^^^..^^^^^...^.^..^^^."
      "^^....^.^...^^^...^^^^..^...^"
      "...^^.^.^.......^..^^...^.^.^"
      "^..^^^^^...^.")
    (collect-rows 400000)
    count-safe
    println))
