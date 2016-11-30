(ns day-1.core
  (:gen-class))

(defn floor
  [coll]
  (reduce
    #(+ %1 (case %2 \( 1 \) -1)) 0 coll))

(defn -main
  [& args]
  (println (floor (first args))))
