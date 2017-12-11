(ns day-10.core
  (:gen-class))

(def ring (->> 256 (range 0) vec))

(def instrs
  (->>
    (->
      (slurp "input.txt")
      (clojure.string/split #"[\s,]+"))
    (map read-string)))

(defn conj-bonus
  [coll]
  (concat coll [17 31 73 47 23]))

(def instrs'
  (->>
    (slurp "input.txt")
    (map int)
    conj-bonus))

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
    (map int)
    conj-bonus
    (c-hash-64 r)
    first
    (partition 16)
    (map (partial apply bit-xor))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
