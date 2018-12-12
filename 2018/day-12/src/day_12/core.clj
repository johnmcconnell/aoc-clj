(ns day-12.core
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


(defn str->row
  [s]
  (as-> s $
    (map-indexed vector $)
    (reduce
      (fn [row [i c]]
        (cond-> row
          (= c \#) (conj i)))
      #{}
      $)))

(defn state->str
  [s]
  (let [idx (apply max s)
        midx (apply min s)]
    (as-> (range midx (inc idx)) $
      (for [i $]
        (if (contains? s i)
          \#
          \.))
      (apply str $))))

(defn ->rule
  [s]
  (let [[l r] (clojure.string/split s #"\s+=>\s+")
        row (str->row l)]
    [row (= "#" r)]))

(def rules
  (as-> my-i' $
    (map ->rule $)
    (into {} $)))

(def e
  "#..#.#..##......###...###")
(def a
  "##..#.#.#..##..#..##..##..#.#....#.....##.#########...#.#..#..#....#.###.###....#..........###.#.#..")

(defn substate
  [state coll]
  (as-> (map-indexed vector coll) $
    (reduce
      (fn [row [idx pos]]
        (cond-> row
          (contains? state pos) (conj idx)))
      #{}
      $)))

(defn next-numbers
  [s]
  (let [s-min (apply min s)
        s-max (apply max s)]
    (range (- s-min 7) (+ s-max 8))))

(defn ->next-state
  [s1 rules]
  (as-> s1 $
    (next-numbers $)
    (reduce
      (fn [s2 i]
        (let [offsets (range -2 3)
              idxs (map (partial + i) offsets)
              v (substate s1 idxs)]
          #_(prn i v)
          (cond-> s2
            (rules v) (conj i))))
      #{}
      $)))

(defn part-1
  [in]
  (let [state (str->row in)
        r (range 0 20)
        rs rules
        ;f state->str
        f #(apply + %)
        ]
    (as-> r $
      (reductions
        (fn [[_ s] i]
          #_(println "iteration:" i)
          [i (->next-state s rs)])
        [0 state]
        $)
      (map second $)
      (map f $))))

(defn state->relative
  [s]
  (let [s-min (apply min s)]
    [s-min (reduce #(conj %1 (- %2 s-min)) #{} s)]))

(defn relative->state
  [[s-min offs]]
  (reduce #(conj %1 (+ %2 s-min)) #{} offs))

(defn part-2
  [in]
  (let [s0 (str->row in)
        l 50000000000M
        r (range 0 l)
        m (quot l (* 5000 1000))
        rs rules
        ;f state->str
        f #(apply + %)]
    (let [v (loop [i 0M
                   state s0
                   cache {}]
              (if (zero? (mod i m)) (println i))
              (if (= i l)
                state
                (let [[m1 offs] (state->relative state)
                      state' (->next-state state rs)
                      [m2 i2] (get cache offs)
                      cache' (assoc cache offs [m1 i])]
                  (if (nil? m2)
                    (recur
                      (inc i)
                      state'
                      cache')
                    (let [x (quot (- l i) (- i i2))
                          i' (- l (rem (- l i) (- i i2)))
                          rel [(+ m1 (* x (- m1 m2))) offs]
                          state' (relative->state rel)]
                      (recur
                        i'
                        state'
                        (dissoc cache offs)))))))]
      (println (state->str v))
      (println (f v))
      v)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
