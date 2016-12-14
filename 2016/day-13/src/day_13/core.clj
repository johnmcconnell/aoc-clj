(ns day-13.core
  (:gen-class))

(defn type-of-pos
  [x y seed]
  (->
    (+
     (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)
     seed)
    Long/bitCount
    even?
    (if :open :wall)))

(defn- st
  [f x]
  (max (f x 1) 0))

(defn next-positions
  [visited x y]
  (->>
    [[(st - x) y] [(st + x) y]
    [x (st - y)] [x (st + y)]]
    (remove #(visited %))
    distinct
    set))

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
