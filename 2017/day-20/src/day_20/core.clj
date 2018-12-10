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
        solve-quad' (fn
                      [a b c]
                      (solve-quad (/ a 2) (+ b (/ a 2)) c))
        rs (for [d dms]
             (->>
               d
               (apply solve-quad')
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
    (prn t)
    (if (< t 0)
      coll
      (let [pairs (get events t)]
        (reduce
          (fn
            [c [v1 v2]]
            (prn t v1 v2)
            (if (and (contains? coll v1)
                     (contains? coll v2)
                     (= (position-at v1 t)
                        (position-at v2 t)))
              (disj c v1 v2)
              c))
          coll
          pairs)))))

(defn combinations
  [coll]
  (if (empty? coll)
    nil
    (let [[x & xs] coll]
      (concat
        (for [y xs] [x y])
        (combinations xs)))))

(defn ->events
  [coll]
  (as-> coll $
    (reduce
      (fn
        [d [its v1 v2]]
        (update d (-> its sort first) conj v1 v2))
      {}
      $)
    (let [e $]
      (reduce
        (fn [coll t]
          (conj coll [t (get e t)]))
        []
        (-> e keys sort)))))

"""
collision at: 11
  [
    {p [14 -9 34], v [-40 -258 -28], a [4 -9 3]}
  p=<758,2493,568>, v=<-88,-150,-64>, a=<4,-9,3>
  [[758 2493 568] [-88 -150 -64] [4 -9 3]]

    {p [14 -9 34], v [140 236 128], a [5 11 5]}
  p=<-1336,-2115,-1172>, v=<80,104,68>, a=<5,11,5>
  [[-1336 -2115 -1172] [80 104 68] [5 11 5]]

    {p [14 -9 34], v [-63 -16 214], a [-1 -3 15]}
  p=<704,-15,-1544>, v=<-51,20,34>, a=<-1,-3,15>

    {p [14 -9 34], v [80 6 95], a [2 -1 3]
  p=<-814,-147,-908>, v=<56,18,59>, a=<2,-1,3>

  }
  ]

  p=<758,2493,568>, v=<-88,-150,-64>, a=<4,-9,3>
  p=<-1336,-2115,-1172>, v=<80,104,68>, a=<5,11,5>
"""

(defn part-2
  []
  (as-> vectors $
    (combinations $)
    (map
      (fn [[v1 v2]]
        (let [its (intersections v1 v2)
              its' (->>
                     its
                     (filter (comp integer? rationalize))
                     (filter pos?))]
          [its' v1 v2]))
      $)
    (map-indexed vector $)
    (for [[i [its v1 v2]] $]
      (do
        (if (zero? (mod i 1000)) (prn "i:" i " of:" (count $)))
        #_(doseq [t its]
          (println "v1:" v1 " intersects with v2:" v2 " at: " t))
        [its v1 v2]))
    (remove (comp empty? first) $)
    (->events $)
    (reduce (fn [vs [t coll]] (apply disj vs coll)) (set vectors) $)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
