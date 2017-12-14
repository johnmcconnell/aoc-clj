(ns day-2.core
  (:gen-class))

(defn ipt
  []
  (->>
    "input.txt"
    slurp
    clojure.string/split-lines
    (map
      (comp
        (partial map read-string)
        #(clojure.string/split % #"[x\s]+")))))

(defn pp-amt
  [[l w h]]
  (let [ss [(* l w) (* w h) (* h l)]]
    (apply + (apply min ss) (map (partial * 2) ss))))

(defn rib-amt
  [dimm]
  (let [ss (take 2 (sort dimm))]
    (+
     (* 2 (apply + ss))
     (apply * dimm))))

(defn total'
  [f ls]
  (reduce
    (fn
      [t dimm]
      (+ t (f dimm)))
    0
    ls))

(defn total-p
  [ls]
  (total' pp-amt ls))

(defn total-rb
  [ls]
  (total' rib-amt ls))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
