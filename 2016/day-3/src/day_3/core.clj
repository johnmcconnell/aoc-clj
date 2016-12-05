(ns day-3.core
  (:gen-class))

(defn parse-triangle
  [input]
  (let [trimmed (clojure.string/trim input)]
    (->> (clojure.string/split trimmed #"\s+")
         (map #(Integer. %)))))

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
  (->> (line-seq (java.io.BufferedReader. *in*))
      (map parse-triangle)
      (filter valid-triangle?)
      count
      println))
