(ns day-6.core
  (:require
    [clojure.set]
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i'
  (clojure.string/split-lines my-i))

(defn ->cord
  [s]
  (let [r (clojure.string/split s #",\s+")]
    (map read-string r)))

(def coords
  (map ->cord my-i'))

(defn char-range
  [start end]
  (map char (range (int start) (inc (int end)))))

(defn ->2dvec
  [x y]
  (vec (replicate x (vec (replicate y nil)))))

(defn abs [n] (max n (- n)))

(defn m-dist
  [x y]
  (as-> (map (comp abs -) x y) $
    (apply + $)))

(defn closest
  [coord coords]
  (->>
    coords
    (map #(vector (m-dist coord %) %))
    (group-by first)
    (sort-by first)
    first
    second))

(def ranges
  (let [xs (map first coords)
        ys (map second coords)]
    [[(apply min xs) (inc (apply max xs))]
     [(apply min ys) (inc (apply max ys))]]))


(defn largest-mark
  [grid]
  (as-> grid $
    (map (fn [[x y]] (map second y)) $)
    (group-by identity $)
    (map (comp count second) $)
    (sort $)
    (last $)))

(defn part-1
  []
  (let [[xr yr] ranges
        xoff (first xr)
        yoff (first yr)]
    (as-> (for [x (apply range xr)
                y (apply range yr)]
            [x y]) $
      (reduce
        (fn
          [grid [x y]]
          (prn "Calculating coord: " x y)
          (assoc
            grid
            [x y]
            (closest [x y] coords)))
        {}
        $)
      #_(for [r $]
        (println r)))))

(defn part-2
  []
  (let [[xr yr] ranges
        xoff (first xr)
        yoff (first yr)]
    (as-> (for [x (apply range xr)
                y (apply range yr)]
            [x y]) $
      (for [coord $]
        (do
          (prn "Finding coord:" coord)
          (apply
            +
            (map (partial m-dist coord) coords))))
      (filter #(< % 10000) $)
      (count $))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
