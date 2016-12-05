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

(defn password-pos-char
  [s]
  (let [valid-num? (set (map str (range 0 8)))
        idx (str (nth s 5))]
    (if (valid-num? idx)
      [(Integer. idx) (nth s 6)]
      false)))

(defn build-str
  [coll]
  (->>
    (map #(coll %) (range 0 8))
    (apply str)))

(defn fill
  [curr [pos chr]]
  ;(println curr pos chr)
  (if (curr pos)
    curr
    (assoc curr pos chr)))

(defn contains-all-pos?
  [hsh]
  (= (set (keys hsh)) (set (range 0 8))))

(defn password-2
  [id]
  (->> (range)
       (map #(str id %))
       (map md5-digest)
       (filter leading-5-0s?)
       (map password-pos-char)
       (filter identity)
       (reductions fill {})
       (filter contains-all-pos?)
       (take 1)
       first
       build-str))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "[" (first args) "]")
  (println (password-2 (first args))))
