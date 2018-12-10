(ns day-11.core
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

(defn power-level
  [[x y] gsn]
  (let [rid  (+ x 10)]
    (as-> rid $
      (* $ y) ; begin
      (+ $ gsn) ; inc
      (* $ rid)
      (mod $ 1000)
      (quot $ 100) ; hundreds
      (- $ 5))))

(defn part-1
  []
  (let [l 300
        gsd 5177
        grid-power (fn
                     [[x y] gsd]
                     (as-> (for [dx (range 0 3)
                                 dy (range 0 3)]
                             [(+ x dx) (+ y dy)]) $
                       (map #(power-level % gsd) $)
                       (reduce + 0 $)))]
    (as-> (for [y (range 1 (- l 1))
                x (range 1 (- l 1))]
            (do (println "x:" x "y:" y)
                [(grid-power [x y] gsd) [x y]])) $)))


(defn grid-power
  [[x y z] gsd]
  (as-> (for [dx (range 0 z)
              dy (range 0 z)]
          [(+ x dx) (+ y dy)]) $
    (map #(power-level % gsd) $)
    (reduce + 0 $)))

(defn cached-grid-power
  [[cache f] [x y z] gsd]
  (if (< z 3)
    (grid-power [x y z] gsd)
    (let [last-d (dec z)
          tr (get cache [(inc x) y last-d])
          bl (get cache [x (inc y) last-d])
          mid (get cache [(inc x) (inc y) (dec last-d)])
          tc (f [x y] gsd)
          bc (f [(+ x last-d) (+ y last-d)] gsd)]
      (- (+ tr bl tc bc) mid))))

(defn part-2
  []
  (let [l 300
        gsd 5177
        plf (memoize power-level)]
    (as-> (for [z (range 0 l)
                y (range 1 (- (inc l) z))
                x (range 1 (- (inc l) z))
                :let [z' (inc z)]]
            [x y z']) $
      (reduce
        (fn
          [[c f [mv mc]] coord]
          (let [[x y _] coord
                v (cached-grid-power [c f] coord gsd)
                [mv' mc'] (if (> v mv)
                               (do
                                 (println "new mv:" v " coord:" coord)
                                 [v mc])
                               [mv mc])]
            (when (= 1 x y) (println "on coord:" coord))
            [(assoc c coord v)
             f
             [mv' mc']]))
        [{} plf [-9999 []]]
        $))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
