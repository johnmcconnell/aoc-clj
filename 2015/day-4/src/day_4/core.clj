(ns day-4.core
  (:import
    java.security.MessageDigest
    java.math.BigInteger)
  (:gen-class))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn first-hash
  [s i x]
  (let [h (md5 (str s i))]
    (if (apply = \0 (take x h))
      [i h]
      (recur s (inc i) x))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
