(ns day-1.core
  (:gen-class))

(defn climb
  [coll]
  (let [floor (reduce
                #(+ %1 (case %2 \( 1 \) -1)) 0 coll)]
    {:floor floor}))

(defn -main
  [& args]
  (println (climb (first args))))
