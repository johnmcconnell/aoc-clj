(ns day-1.core
  (:gen-class))

(defn floor
  [coll]
  (reduce
    #(+ %1 (case %2 \( 1 \) -1)) 0 coll))

(defn pos
  [coll]
  (count
    (take-while
      (partial not= -1)
      (reductions
        #(+ %1 (case %2 \( 1 \) -1)) 0 coll))))

(defn climb
  [coll]
  {:floor (floor coll)
   :pos (pos coll)})

(defn -main
  [& args]
  (println (climb (first args))))
