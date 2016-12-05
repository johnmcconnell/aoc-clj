(ns day-3.core
  (:gen-class))

(defn parse-triangle
  [input]
  (let [trimmed (clojure.string/trim input)]
    (->> (clojure.string/split trimmed #"\s+")
         (map #(Integer. %)))))

(defn collect-triangles
  [coll [col1 col2 col3]])

(defn collect-triangles
  [coll triplet]
  (let [t1 (map #(nth % 0) triplet)
        t2 (map #(nth % 1) triplet)
        t3 (map #(nth % 2) triplet)
        triangles [t1 t2 t3]]
    (concat coll triangles)))

(defn parse-by-column
  [input]
  (->>
    input
    clojure.string/split-lines
    (map parse-triangle)
    (partition 3)
    (reduce collect-triangles [])))

(defn fst-2-elems-gt-eq
  [coll]
  (>
    (+ (nth coll 0) (nth coll 1))
    (nth coll 2)))

(defn valid-triangle?
  [triangle]
  (every?
    fst-2-elems-gt-eq
    (take
      3
      (partition 3 1 (cycle triangle)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (slurp *in*)
    parse-by-column
    (filter valid-triangle?)
    count
    println))
