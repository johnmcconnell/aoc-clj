(ns day-1.core
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i' (clojure.string/split-lines my-i))

(def nums (map read-string my-i'))

(defn part-1
  []
  (reduce + nums))

(defn part-2
  []
  (as-> (cycle nums) $
    (reductions
      (fn
        [[c f] d]
        (println "Searching: " (count c))
        (let [v (+ f d)]
          (if (contains? c v)
            [v v]
            [(conj c v) v])))
      [#{0} 0]
      $)
    (drop-while (comp set? first) $)
    (first $)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
