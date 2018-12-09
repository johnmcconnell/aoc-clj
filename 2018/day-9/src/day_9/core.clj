(ns day-9.core
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

(defn scores
  [players last-marble]
  (loop [cm 1
         p-idx 0
         c-node {:val 0 :l 0 :r 0}
         nodes {0 c-node}
         scores {}]
    (if (zero? (mod cm 10000)) (println "working on: " cm " of: " last-marble))
    (cond
      (> cm last-marble) (do
                           #_(clojure.pprint/pprint nodes)
                           #_(clojure.pprint/pprint (->list nodes))
                           scores)
      (-> cm (mod 23) zero?) (let [tbr (ll/move nodes c-node :l 7)
                                   nodes (ll/remove nodes tbr)
                                   c-node (ll/right-of nodes tbr)
                                   scores (update scores p-idx (fnil + 0) cm (:val tbr))]
                               (recur
                                 (inc cm)
                                 (-> p-idx inc (mod players))
                                 c-node
                                 nodes
                                 scores))
      :else (let [shift-node (ll/move nodes c-node :r 1)
                  [nodes node] (ll/push-right nodes shift-node cm)
                  cm' (inc cm)]
              (recur
                (inc cm)
                (-> p-idx inc (mod players))
                node
                nodes
                scores)))))

(defn part-1
  []
  (as-> (scores 441 71032) $
    (map second $)
    (apply max $)))

(defn part-2
  []
  (as-> (scores 441 (* 100 71032)) $
    (map second $)
    (apply max $)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
