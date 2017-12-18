(ns day-16.core
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(defn div-args
  [coll]
  (->>
    #"/"
    (clojure.string/split (apply str coll))
    (map read-string)))

(defn div-args'
  [coll]
  (->>
    #"/"
    (clojure.string/split (apply str coll))
    (map clojure.string/lower-case)
    (map first)))

(defn ->spin
  [xs]
  (let [x (read-string (apply str xs))]
    (fn
      [pd nd]
      (let [l (count pd)
            pd' (reduce
                  (fn
                    [h [k v]]
                    (assoc h (mod (+ k x) l) v))
                  {}
                  pd)]
      [pd' (clojure.set/map-invert pd')]))))

(defn ->exchange
  [xs]
  (let [[a b] (div-args xs)]
    (fn
      [pd nd]
      (let [pd' (->
                  pd
                  (assoc a (get pd b))
                  (assoc b (get pd a)))]
        [pd' (clojure.set/map-invert pd')]))))

(defn ->partner
  [xs]
  (let [[a b] (div-args' xs)]
    (fn
      [pd nd]
      (let [nd' (->
                  nd
                  (assoc a (get nd b))
                  (assoc b (get nd a)))]
        [(clojure.set/map-invert nd') nd]))))

(defn ->cmd
  [[c & xs]]
  (condp = c
    \s (->spin xs)
    \x (->exchange xs)
    \p (->partner xs)))

(defn ipt
  [i]
  (->>
    (clojure.string/split i #",")
    (map ->cmd)))

(defn ->states
  [r]
  (let [x (->>
            r
            (map-indexed vector)
            (reduce
              (fn [h [i n]]
                (assoc h i n))
              {}))]
    [x (clojure.set/map-invert x)]))

(defn dance'
  [cmds [pos names]]
  (->>
    cmds
    (reduce
      (fn [[nd pd] f]
        (f nd pd))
      [pos names])))

(defn dance
  [r cmds]
  (let [[pos names] (->states r)]
    (->>
      cmds
      (reduce
        (fn [[nd pd] f]
          (f nd pd))
        [pos names]))))

(defn d->str
  [[ps _]]
  (->>
    ps
    (reduce
      (fn
        [v' [k v]]
        (assoc v' k v))
      (vec (range 0 (count ps))))
    (apply str)))

(def one-b (* 1 1000 1000 1000))

(defn find-id
  [r cmds si]
  (let [[s-pos s-names] (->states r)]
    (->>
      (iterate
        (fn [[[s i] pos names]]
          (let [pos-str (d->str [pos])
                idx (get s pos-str)]
            (prn i pos-str)
            (if (nil? idx)
              (let [[pos' names'] (dance' cmds [pos names])]
                [[(assoc s (d->str [pos]) i) (dec i)] pos' names'])
              (do (prn s pos-str (- idx i)) [[{} (rem i (- idx i))] pos names]))))
        [[{} si] s-pos s-names])
      (map-indexed
        (fn
          [i v]
          (if (= (mod i 100) 0)
            (prn i (-> v first count) (-> v second)))
          v))
      (drop-while (comp (partial < 0) second first))
      first
      second
      vector
      d->str)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
