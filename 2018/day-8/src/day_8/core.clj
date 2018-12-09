(ns day-8.core
  (:require
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

(def nums
  (as-> my-i $
    (clojure.string/split $ #"\s+")
    (map clojure.edn/read-string $)))

(defn parse-nodes'
  [nodes coll i n]
  (if (zero? n)
    [nodes i]
    (let [[c-cnt m-cnt] (subvec coll i)
          [children i'] (parse-nodes' [] coll (+ i 2) c-cnt)
          i'' (+ i' m-cnt)
          metas (subvec coll i' i'')
          self {:children (reverse children) :metas metas}]
      (parse-nodes' (cons self nodes) coll i'' (dec n)))))

(defn parse-nodes
  [coll n]
  (first (parse-nodes' [] coll 0 n)))

(defn nmap
  [f {:keys [children] :as self}]
  (concat
    [(f self)]
    (map (partial nmap f) children)))

(defn tap
  [x]
  (prn x)
  x)

(defn score
  [{:keys [children metas] :as self}]
  (if (empty? children)
    (apply + metas)
    (let [vs (mapv score children)]
      (as-> vs $
        (map #(nth $ (dec %) 0) metas)
        (apply + $)))))

(defn part-1
  []
  (as-> nums $
    (parse-nodes (vec $) 1)
    (first $)
    (nmap (fn [{:keys [metas]}] (apply + metas)) $)
    (flatten $)
    (apply + $)))

(defn part-2
  []
  (as-> nums $
    (parse-nodes (vec $) 1)
    (first $)
    (score $)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
