(ns day-5.core
  (:require
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

(def matches
  (let [x (char-range \a \z)
        y (char-range \A \Z)]
    (->> (mapcat (fn [a b] [[a b] [b a]]) x y) set)))

(defn compress-polys
  [s]
  (as-> s $
    (reduce
      (fn
        [[s p m] n]
         (cond
           (nil? p) [s n m]
           (contains? matches [p n]) [s nil true]
           :else [(conj s p) n m]))
      [[] nil false]
      (conj s 1))
    [(vec (first $)) (nth $ 2)]))

(defn compress-all
  [s]
  (loop [v (vec s)
         done? false]
    (if done?
      (apply str v)
      (let [[v' changes?] (compress-polys v)]
        (recur v' (not changes?))))))

(defn part-1
  []
  (->>
    (compress-all my-i)
    count))

(defn part-2
  []
  (for [c (char-range \a \z)]
    (do
      (prn "trying: " c)
      (as-> (clojure.string/upper-case c) $
        (str c "|" $)
        (re-pattern $)
        (clojure.string/replace my-i $ "")
        (compress-all $)
        [(count $) c]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
