(ns day-16.core
  (:gen-class))

(defn flip-c
  [c]
  (condp = c
    \0 \1
    \1 \0))

(defn flip
  [s]
  (->>
    s
    reverse
    (map flip-c)
    (apply str)))

(defn dragon
  [s]
  (str s "0" (flip s)))

(defn pair-flip
  [[a b]]
  (if (= a b)
    \1
    \0))

(defn trunc-pairs
  [s]
  (->>
    s
    (partition 2)
    (map pair-flip)
    (apply str)))

(defn checksum
  [s]
  (if (= (mod (count s) 2) 1)
    s
    (checksum (trunc-pairs s))))

(defn fill
  [s cnt]
  (->>
    (iterate dragon s)
    (drop-while #(< (count %) cnt))
    first
    (take cnt)
    checksum))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (fill "11110010111001001" 35651584)
    println))
