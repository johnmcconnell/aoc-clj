(ns day-20.core
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

(defn quad-zeros
  [[a b c]]
  (if (= 0 a b c)
    #{:*}
    #{}))

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

(defn position-at
  [v t]
  (as-> (range 0 3) $
    (for [i $]
      (map #(nth % i) v))
    (for [[c b a] $]
      (+
       (/ (* t t a) 2)
       (* t b)
       c))))

(defn intersections
  [v1 v2]
  (let [->d (fn
              [coll d]
              (map #(nth % d) coll))
        dms (for [i (range 0 3)]
              (->>
                (map - (->d v1 i) (->d v2 i))
                reverse))
        rs (for [d dms]
             (->>
               d
               (apply solve-quad)
               #_(map
                 #(if (contains? #{nil :*} %) % (Math/round (double %))))
               set))
        rs-matter (remove #(contains? % :*) rs)]
    (if (empty? rs-matter)
      #{:*}
      (apply clojure.set/intersection rs-matter))))

(defn part-1
  []
  (as-> vectors $
    (for [[p v a] $]
      (as-> a $
        (map abs $)
        [(reduce + 0 $) [p v a]]))
    (map-indexed vector $)
    (sort-by (comp first second) $)))

(defn simulate
  [events]
  (fn
    [coll t]
    (let [pairs (get events t)]
      (reduce
        (fn
          [c [v1 v2]]
          (if (and (contains? coll v1) (contains? coll v2))
            (disj c v1 v2)
            c))
        coll
        pairs))))

(defn part-2
  []
  (as-> vectors $
    (for [v1 $]
      (let [intersects (for [v2 $
                             :when (not= v1 v2)]
                         [v2 (intersections v1 v2)])]
        [v1 (remove (comp empty? second) intersects)]))
    (map-indexed vector $)
    (for [[i [v1 its]] $]
      (do
        (prn "i:" i " of:" (count $))
        (doseq [[v2 t] its]
          (println "v1:" v1 " intersects with v2:" v2 " at: " t))
        [v1 its]))
    (reduce
      (fn
        [e1 [v1 its]]
        (reduce
          (fn [e2 [v2 ts]]
            (reduce
              (fn [e3 t]
                (update e3 t conj [v1 v2]))
              e2
              ts))
          e1
          its))
      {}
      $)
    (let [events $]
      (reduce
        (simulate events)
        (set vectors)
        (->>
          events
          keys
          sort
          (filter integer?))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
