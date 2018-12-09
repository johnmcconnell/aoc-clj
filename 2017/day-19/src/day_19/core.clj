(ns day-19.core
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

#_(def nums
  (as-> my-i $
    (clojure.string/split $ #"\s+")
    (map clojure.edn/read-string $)))

(defn nmap
  [f {:keys [children] :as self}]
  (concat
    [(f self)]
    (map (partial nmap f) children)))

(defn tap
  [x]
  (prn x)
  x)

(def grid
  (->
    (for [line my-i']
      (->
        (for [c line]
          c)
        vec))
    vec))

(defn ->simplified-grid
  [grid coord d]
  (loop [nodes [{:coord coord :value :start}]
         c coord
         pdir nil
         dir d
         i 0]
    (let [[x y] c
          has-value? (fn [x] (not= \space x))
          dxs {:d [0] :u [0] :l [-1] :r [1] :* [0 0 -1 1]}
          dys {:d [1] :u [-1] :l [0] :r [0] :* [1 -1 0 0]}
          p-dxs {:d [0 -1 1]
                 :u [0 -1 1]
                 :l [0 0 -1]
                 :r [0 0 1]}
          p-dys {:d [1 0 0]
                 :u [-1 0 0]
                 :l [1 -1 0]
                 :r [1 -1 0]}
          dxs (cond-> dxs
                (= dir :*)
                (assoc :* (p-dxs pdir)))
          dys (cond-> dys
                (= dir :*)
                (assoc :* (p-dys pdir)))
          dxs' (dxs dir)
          dys' (dys dir)
          direction-of (fn
                         [c1 c2]
                         (as-> (map - c1 c2) $
                           (cond
                             (-> $ first (> 0)) :l
                             (-> $ first (< 0)) :r
                             (-> $ second (> 0)) :u
                             (-> $ second (< 0)) :d)))
          candidates (map
                       (fn
                         [dx dy]
                         (let [coord' [(+ x dx) (+ y dy)]
                               v (get-in grid (reverse coord'))]
                           [coord' v]))
                       dxs'
                       dys')
          [coord' v] (->>
                       candidates
                       (filter (comp has-value? second))
                       first)
          dir' (cond
                 (= v \+) :*
                 (= dir :*) (direction-of c coord')
                 :else dir)
          nodes' (cond-> nodes
                  (->> v (contains? #{\space \- \| \+}) not)
                  (conj {:coord coord' :value v}))]
      (if (nil? coord')
        [nodes (inc i)]
        (recur
          nodes'
          coord'
          dir
          dir'
          (inc i))))))

(defn part-1
  []
  (as-> grid $
    (->simplified-grid $ [153 0] :d)))

(defn part-2
  []
  nil)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
