(ns day-5.core
  (import java.security.MessageDigest
          java.math.BigInteger)
  (:gen-class))

; https://gist.github.com/jizhang/4325757
(defn md5-digest
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn leading-5-0s?
  [s]
  (= "00000" (subs s 0 5)))

(defn password
  [id]
  (->> (range)
       (map #(str id %))
       (map md5-digest)
       (filter leading-5-0s?)
       (take 8)
       (map #(nth % 5))
       (apply str)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
