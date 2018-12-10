(ns day-10.core
  (:require
    [linked-list.core :as ll]
    [clojure.pprint]
    [clojure.set]
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i'
  (clojure.string/split-lines my-i))

(def vectors
  (letfn [(->v
            [l]
            (for [pts (re-seq #"<.*?>" l)]
              (map read-string (re-seq #"-?\d+" pts))))]
    (map ->v my-i')))

(defn char-range
  [start end]
  (map char (range (int start) (inc (int end)))))

(defn ->2dvec
  [x y]
  (vec (replicate x (vec (replicate y nil)))))

(defn abs
  [n]
  (max n (- n)))

(defn m-dist
  [x y]
  (as-> (map (comp abs -) x y) $
    (apply + $)))

(def vectors
  (letfn [(->v
            [l]
            (for [pts (re-seq #"<.*?>" l)]
              (map read-string (re-seq #"-?\d+" pts))))]
    (map ->v my-i')))

(defn nmap
  [f {:keys [children] :as self}]
  (concat
    [(f self)]
    (map (partial nmap f) children)))

(defn tap
  [x]
  (prn x)
  x)

(defn solve-linear
  [a b]
  (if (zero? a)
    (if (zero? b)
      :*
      nil)
    (/ (- b) a)))

(defn solve-quad
  [a b c]
  (let [t1 (- 0 b)
        t2 (Math/sqrt (- (* b b) (* 4 a c)))
        t3 (* 2 a)
        nan? (fn [x] (and (number? x) (Double/isNaN x)))]
    (cond
      (zero? a) (set (remove nan? [(solve-linear b c)]))
      :else (set (remove nan? [(/ (- t1 t2) t3) (/ (+ t1 t2) t3)])))))

(defn combinations
  [coll]
  (if (empty? coll)
    nil
    (let [[x & xs] coll]
      (concat
        (for [y xs] [x y])
        (combinations xs)))))

(defn to-file
  [s i]
  (spit (str "resources/outputs/vectors." i ".out") s))

(defn to-console
  [s i]
  (println s))

(defn print-vectors
  [vs i]
  (let [xs (map (comp first first) vs)
        ys (map (comp second first) vs)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)
        xd (abs (- max-x min-x))
        yd (abs (- max-y min-y))
        dist (+ xd yd)]
    (println "=== Iteration:" i "[[" min-x ", " max-x "], ["min-y ", " max-y "]]:" dist xd yd "====")
    (if (and (< (abs (- xd yd)) 100) (< xd 200) (< yd 200))
      (do
        #_(spit (str "resources/vectors/vector." i ".edn") (with-out-str (pr vs)))
        (let [v (reduce
                  (fn [s [x y]]
                    (let [v (if (contains? vs [x y])
                              "#"
                              " ")]
                      (cond-> (str s v)
                        (= (inc x) max-x) (str "\n"))))
                  ""
                  (for [y (range min-y (inc max-y))
                        x (range min-x (inc max-x))]
                    [x y]))]
          (to-console v i)))
      (println "Skipping too large:" dist))
    dist))

(defn ->tick
  [vs]
  (reduce
    (fn
      [d1 [p vs]]
      (reduce
        (fn [d2 v]
          (let [p' (map + p v)]
            (update d2 p' conj v)))
        d1
        vs))
  {}
  vs))

(defn print-from-disk
  []
  (doseq [f (drop 1 (file-seq (clojure.java.io/file "resources/vectors")))]
    (prn f)
    (prn (.getName f))
    (let [d (-> f slurp read-string)]
      (print-vectors d (.getName f)))))


(defn part-1
  []
  (loop [i 0
         vs (reduce (fn [d [p v]] (update d p conj v)) {} vectors)]
    (print-vectors vs i)
    (recur
      (inc i)
      (->tick vs))))

(defn part-2
  []
  nil)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
