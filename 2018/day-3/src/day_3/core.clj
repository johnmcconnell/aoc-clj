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

(defn conj-sqr
  [i [id [x y] [w h]]]
  (let [positions (for [dx (range 0 w)
                        dy (range 0 h)]
                    [(+ x dx) (+ y dy)])]
    (reduce
      (fn [[ids s] pos]
        (let [s' (update-in s pos conj id)
              ins (get-in s' pos)
              ids' (if (> (count ins) 1)
                     (apply disj ids ins)
                     ids)]
            [ids' s']))
      i
      positions)))

(defn part-2
  []
  (let [l (range 0 1000)
        ids (set (map first claims))
        sqr (->>
              (for [_ l]
                (->>
                  (for [_ l]
                    #{})
                  vec))
              vec)]
    (as-> sqr $
      (reduce
        conj-sqr
        [ids $]
        claims)
      (first $))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
