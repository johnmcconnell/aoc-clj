(ns day-7.core
  (:gen-class))

(defn g-input
  []
  (->>
    (slurp "input.txt")
    clojure.string/split-lines
    (map #(clojure.string/split % #"[,\s]+"))))

(def i-struct
  (->>
    (g-input)
    (reduce
      (fn
        [h [tn w i & pars]]
        (if (nil? i)
          (assoc h (keyword tn) nil)
          (assoc h (keyword tn) (map keyword pars))))
      {})))

(def w-struct
  (->>
    (g-input)
    (reduce
      (fn
        [h [tn w i & pars]]
        (if (nil? i)
          (assoc h (keyword tn) [(first (read-string w)) nil])
          (assoc h (keyword tn) [(first (read-string w)) (map keyword pars)])))
      {})))

(defn depth-of
  [s cnt n]
  (let [pars (get s n)]
    (if (empty? pars)
      cnt
      (let [ds (map (partial depth-of s (inc cnt)) pars)]
        (apply max ds)))))

(defn depths-of
  [s]
  (reduce
    (fn
      [h tn]
      (assoc h tn (depth-of s 0 tn)))
    {}
    (keys s)))

(defn max-height [] (apply max-key val (depths-of i-struct)))

(defn weight-of
  [s n]
  (let [[wt pars] (get s n)]
    (if (empty? pars)
      wt
      (let [ds (map (partial weight-of s) pars)]
        (if (apply not= ds) (prn n pars ds))
        (apply + wt ds)))))

(defn weights-of
  [s]
  (reduce
    (fn
      [h tn]
      (assoc h tn (weight-of s tn)))
    {}
    (keys s)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (weights-of w-struct)))
