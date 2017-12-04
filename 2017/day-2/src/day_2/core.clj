(ns day-2.core
  (:gen-class))

(defn part-1
  [rows]
  (reduce
    (fn
      [s row]
      (let [mn (apply min row)
            mx (apply max row)]
        (+ s (- mx mn))))
    0
    rows))

(defn divisor-of
  [nums]
  (fn
    [n]
    (->>
      (map
        (fn
          [m]
          (if (= (mod n m) 0)
            (quot n m)
            nil))
        (disj nums n))
      (remove nil?)
      first)))

(defn divisor-in
  [nums]
  (->>
    nums
    (map (divisor-of nums))
    (remove nil?)
    first))

(defn part-2
  [rows]
  (reduce
    (fn
      [s row]
      (let [nums (set row)
            div (divisor-in nums)]
        (+ s div)))
    0
    rows))

(defn ->rows
  [coll]
  (->>
    coll
    (map
      (fn
        [row]
        (->>
          (clojure.string/split row #"\s+")
          (map read-string))))))

(defn parse-it
  "I don't do a whole lot ... yet."
  [in]
  (->>
    in
    clojure.string/split-lines
    ->rows))

(defn- main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    clojure.string/split-lines
    ->rows
    part-1
    println))
