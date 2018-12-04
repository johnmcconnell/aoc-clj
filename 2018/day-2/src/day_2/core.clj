(ns day-2.core
  (:require
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i' (clojure.string/split-lines my-i))

(def freqs
  (as-> my-i' $
    (map frequencies $)
    (map #(group-by second %) $)
    (map
      #(reduce-kv
         (fn
           [m k v]
           (assoc m k (set (map first v))))
         {}
         %)
      $)))

(defn part-1
  []
  (as-> freqs $
    (reduce
      (fn
        [cnts f]
        (cond-> cnts
          (contains? f 2) (update 0 inc)
          (contains? f 3) (update 1 inc)))
      [0 0]
      $)
    (apply * $)))

(defn similar?
  [s1 s2]
  (let [->v #(map-indexed vector %)
        v1 (->v s1)
        v2 (->v s2)
        [a b _] (clojure.data/diff v1 v2)
        r1 (->> (flatten a) (remove nil?))
        r2 (->> (flatten b) (remove nil?))]
    #_(println (count r1) (count r2))
    (= (count r1) (count r2) 1)))

(defn part-2
  []
  (for [w my-i']
    (filter #(similar? w %) my-i')))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
