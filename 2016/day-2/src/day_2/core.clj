(ns day-2.core
  (:gen-class))

(def up->
  {1 1 2 2 3 3
   4 1 5 2 6 3
   7 1 8 2 9 3})

(def left->
  {1 1 2 1 3 2
   4 1 5 1 6 2
   7 1 8 1 9 2})

(defn button-move
  [curr move]
  (case move
    "U" (up-> curr)
    "L" (left-> curr)))

(defn final-button
  [init & moves]
  (reduce button-move init moves))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
