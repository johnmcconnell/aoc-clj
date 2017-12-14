(ns day-14.core
  (:gen-class))

(def ring (->> 256 (range 0) vec))

(defn conj-bonus
  [coll]
  (concat coll [17 31 73 47 23]))

(defn idx-of
  [coll i]
  (mod i (count coll)))

(defn items-of
  [coll s e]
  (->>
    (range s e)
    (map
      (fn
        [i]
        (let [i' (idx-of coll i)]
          [i' (nth coll i')])))))

(defn rev-range
  [v s e]
  (let [items (items-of v s e)
        poses (->> items
                   (map first)
                   reverse)
        items' (->> items
                    (map second)
                    (map vector poses))]
    (reduce
      (fn
        [v' [i va]]
        (assoc v' i va))
      v
      items')))

(defn c-hash
  [r itrs]
  (reduce
    (fn
      [[h idx sk] l]
      [(rev-range h idx (+ idx l))
       (+ idx l sk)
       (inc sk)])
    [r 0 0]
    itrs))

(defn c-hash'
  [r itrs idx sk]
  (reduce
    (fn
      [[h idx' sk'] l]
      [(rev-range h idx' (+ idx' l))
       (+ idx' l sk')
       (inc sk')])
    [r idx sk]
    itrs))

(defn c-hash-64
  [r itrs]
  (reduce
    (fn
      [[r' idx sk] _]
      (c-hash' r' itrs idx sk))
    [r 0 0]
    (range 0 64)))

(defn unhexify
  [coll]
  (->>
    coll
    (map (partial format "%02x"))
    (apply str)))

(defn c-hash-nums
  [r itrs]
  (->>
    itrs
    conj-bonus
    (c-hash-64 r)
    first
    (partition 16)
    (map (partial apply bit-xor))))

(defn knot-hash
  [s]
  (->>
    s
    (map int)
    (c-hash-nums ring)
    unhexify))

(def ipt "jzgqcdpd")

(defn ->h
  [s i]
  (str s "-" i))

(defn pad
  [coll]
  (if (< (count coll) 4)
    (pad (str "0" coll))
    coll))

(defn ->b
  [hex]
  (->>
    hex
    (map
      (fn
        [c]
        (->
          (Integer/parseInt (str c) 16)
          (Integer/toString 2)
          pad)))
    (map (partial apply vector))
    flatten
    (map {\1 1 \0 0})))

(defn used-cnt
  [s]
  (->>
    (range 0 128)
    (reduce
      (fn
        [l i]
        (->>
          (->h s i)
          knot-hash
          ->b
          (concat l)))
      [])
    frequencies))

(defn positions
  [s]
  (->>
    (range 0 128)
    (reduce
      (fn
        [h1 i]
        (prn i)
        (->>
          (->h s i)
          knot-hash
          ->b
          (map-indexed vector)
          (reduce
            (fn
              [h2 [j v]]
              (assoc h2 [i j] v))
            h1)))
      {})))

(defn used-posses
  [s]
  (->>
    s
    (filter (comp (partial = 1) second))
    (map first)
    set))

(defn tap
  [x]
  (prn x)
  x)

(defn neighbors
  [[x y] r]
  (->>
    (mapcat
      (fn [i]
        (if (= i 0)
          []
          [(get r [(+ x i) y])
           (get r [x (+ y i)])]))
      (range -1 2))
    (remove nil?)))

(defn g-class
  [r c [o & os]]
  (if (nil? o)
    [r c]
    (let [nbrs (neighbors o r)
          r' (clojure.set/difference r (conj nbrs o))
          c' (apply conj c o nbrs)
          os' (apply conj os nbrs)]
      (recur r' c' os'))))

(defn g-classes
  [r cs]
  (if (= (count r) 0)
    cs
    (let [x (first r)
          [r' c] (g-class r #{} [x])]
      (recur r' (conj cs c)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
