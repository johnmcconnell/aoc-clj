(ns day-3.core
  (:gen-class))

(defn ->jumps
  [coll]
  (map
    (fn
      [c]
      (condp = c
        \> [1 0]
        \< [-1 0]
        \v [0 -1]
        \^ [0 1]))
    coll))

(defn ipt
  []
  (->>
    "input.txt"
    slurp
    ->jumps))

(defn houses
  [coll]
  (->>
    coll
    (reduce
      (fn [[s coord] jump]
        (let [c (map + coord jump)]
          [(conj s c) c]))
      [#{[0 0]} [0 0]])))

(defn robo-houses
  [coll]
  (->>
    coll
    (reduce
      (fn [[s [cd1 cd2]] jump]
        (let [c (map + cd1 jump)]
          [(conj s c) [cd2 c]]))
      [#{[0 0]} [[0 0] [0 0]]])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
