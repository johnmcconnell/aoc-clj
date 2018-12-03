(ns day-3.core
  (:require
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i' (clojure.string/split-lines my-i))

(defn ->claim
  "#1 @ 861,330: 20x10"
  [s]
  (let [[id _ pos size] (clojure.string/split s #"\s+")
        ->n #(map read-string %)
        [x y] (clojure.string/split pos #",|:")
        [w h] (clojure.string/split size #"x")]
    [id (->n [x y]) (->n [w h])]))

(def claims
  (map ->claim my-i'))

(defn inc-sqr
  [sqr [id [x y] [w h]]]
  (let [positions (for [dx (range 0 w)
                        dy (range 0 h)]
                    [(+ x dx) (+ y dy)])]
    (reduce
      (fn [s pos]
        (update-in s pos inc))
      sqr
      positions)))

(defn part-1
  []
  (let [l (range 0 1000)
        sqr (->>
              (for [_ l]
                (->>
                  (for [_ l]
                    0)
                  vec))
              vec)]
    (as-> sqr $
      (reduce
        inc-sqr
        $
        claims)
      (flatten $)
      (filter #(> % 1) $)
      (count $))))

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
