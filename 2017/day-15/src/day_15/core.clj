(ns day-15.core
  (:gen-class))

(defn ->gen
  [m]
  (fn
    [v]
    (rem (* m v) 2147483647)))

(def ag (->gen 16807))
(def bg (->gen 48271))

(defn ->gen-2
  [g x]
  (fn
    [v]
    (let [v' (g v)]
      (if (= (mod v' x) 0)
        v'
        (recur v')))))

(def ag-2 (->gen-2 ag 4))
(def bg-2 (->gen-2 bg 8))

(def gens [ag bg])
(def gens-2 [ag-2 bg-2])

(def strts [873 583])
(def strts' [65 8921])

(defn ->b
  [i]
  (Integer/toString i 2))

(defn pad
  [coll]
  (if (< (count coll) 32)
    (pad (str "0" coll))
    coll))

(defn calcs
  [gens start cnt]
  (->>
    start
    (iterate
      (fn
        [vs]
        (map
          (fn [g v] (g v))
          gens
          vs)))
    (drop 1)
    (take cnt)
    (map
      (->>
        ->b
        (comp pad)
        (partial map)))))

(def forty-m (* 40 1000 1000))
(def five-m (* 5 1000 1000))

(defn print-it
  []
  (doseq [vs (calcs gens-2 strts' 5)]
    (->>
      vs
      (map #(subs % 16))
      (apply =)
      (println vs))))

(defn eq-16
  [vs]
  (->>
    vs
    (map #(subs % 16))
    (apply =)))

(defn cnt-same
  []
  (->>
    (calcs gens strts forty-m)
    (map-indexed vector)
    (reduce
      (fn
        [a [i vs]]
        (if (= (mod i 100000) 0)
          (prn i))
        (if (eq-16 vs)
          (inc a)
          a))
      0)))

(defn cnt-same-2
  []
  (->>
    (calcs gens-2 strts' five-m)
    (map-indexed vector)
    (reduce
      (fn
        [a [i vs]]
        (if (= (mod i 100000) 0)
          (prn i))
        (if (eq-16 vs)
          (inc a)
          a))
      0)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
