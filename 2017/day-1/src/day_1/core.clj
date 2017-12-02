(ns day-1.core
  (:gen-class))

(defn count-it'
  "I don't do a whole lot."
  [coll]
  (->>
    coll
    (partition 2 1 coll)
    (reduce
      (fn [s [a b]]
        (if (= a b)
          (+ s a)
          s))
      0)))

(defn count-it
  "I don't do a whole lot."
  [coll]
  (->>
    coll
    (map (comp read-string str))
    count-it'))

(defn count-it''
  [coll]
  (->>
    coll
    (reduce
      (fn [s [a b]]
        (if (= a b)
          (+ s a)
          s))
      0)))

(defn count-it-2
  "I don't do a whole lot."
  [coll]
  (let [coll' (map (comp read-string str) coll)
        l (count coll')
        idx (quot l 2)
        coll1 (take l coll')
        coll2 (take l (drop idx (cycle coll')))]
    (->>
      (map vector coll1 coll2)
      count-it'')))


(defn- main
  [& args]
  (->>
    *in*
    slurp
    count-it
    println))
