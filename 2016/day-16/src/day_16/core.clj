(ns day-16.core
  (:gen-class))

(defn checksum
  [s]
  s)

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

(defn fill
  [s cnt]
  (->>
    (iterate dragon s)
    (drop-while #(< (count %) cnt))
    first
    checksum))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
