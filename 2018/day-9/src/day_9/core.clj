(ns day-9.core
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

(defn ll-fetch-of
  [nodes node k]
  (->
    node
    k
    nodes))

(defn ll-right-of
  [nodes node]
  (fetch-of nodes node :r))

(defn ll-left-of
  [nodes node]
  (fetch-of nodes node :l))

(defn ll-push-right
  [nodes left-n v]
  #_(clojure.pprint/pprint nodes)
  (let [l-v (:val left-n)
        l-r (:r left-n)
        m-n {:val v
             :l l-v
             :r l-r}
        right-n (-> (ll-right-of nodes left-n) (assoc :l v))
        left-n (if (= (:val right-n) (:val left-n))
                 (assoc left-n :l v :r v)
                 (assoc left-n :r v))
        nodes' (assoc
                 nodes
                 (:val right-n) right-n
                 (:val left-n) left-n
                 v m-n)]
    [nodes' left-n m-n]))

(defn ->list
  [nodes]
  (loop [seen #{}
         node (-> nodes first second)
         rs []]
    (if (or (nil? node) (contains? seen (:val node)))
      rs
      (let [v (:val node)
            nxt (ll-right-of nodes node)
            rs' (conj rs v)]
        (recur
          (conj seen v)
          nxt
          rs')))))

(defn ll-move
  [nodes node dir cnt]
  (if (zero? cnt)
    node
    (recur
      nodes
      (ll-fetch-of nodes node dir)
      dir
      (dec cnt))))

(defn ll-remove-node
  [nodes node]
  (let [v (:val node)
        l (left-of nodes node)
        r (right-of nodes node)
        l (if (= (:val l) (:val r))
            (assoc
              l
              :r (:val r)
              :l (:val r))
            (assoc l :r (:val r)))
        r (assoc r :l (:val l))
        nodes (->
                nodes
                (dissoc v)
                (assoc
                  (:val r) r
                  (:val l) l))]
    nodes))

(defn scores
  [players last-marble]
  (loop [cm 1
         p-idx 0
         c-node {:val 0 :l 0 :r 0}
         nodes {0 c-node}
         scores {}]
    (cond
      (> cm last-marble) (do
                           #_(clojure.pprint/pprint nodes)
                           #_(clojure.pprint/pprint (->list nodes))
                           scores)
      (-> cm (mod 23) zero?) (let [tbr (ll-move nodes c-node :l 7)
                                   nodes (ll-remove-node nodes tbr)
                                   c-node (right-of nodes tbr)
                                   scores (update scores p-idx (fnil + 0) cm (:val tbr))]
                               (recur
                                 (inc cm)
                                 (-> p-idx inc (mod players))
                                 c-node
                                 nodes
                                 scores))
      :else (let [shift-node (ll-move nodes c-node :r 1)
                  [nodes _ node] (ll-push-right nodes shift-node cm)
                  cm' (inc cm)]
              (recur
                (inc cm)
                (-> p-idx inc (mod players))
                node
                nodes
                scores)))))

(defn part-1
  []
  (scores 441 71032))

(defn part-2
  []
  nil)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
