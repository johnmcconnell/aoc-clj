(ns day-14.core
	(:import java.security.MessageDigest
           java.math.BigInteger)
  (:gen-class))

(defn md5
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn hashes
  [s]
  (map #(md5 (str s %)) (range)))

(defn conj-triples-of
  [triples idx hsh]
  (let [[m s] (->>
                hsh
                (re-find #"(\w)\1{2,}"))]
    (if (nil? s)
      triples
      (update-in triples [s] #(set (conj % idx))))))

(defn conj-kys-of
  [kys triples idx hsh]
  (->>
    hsh
    (re-seq #"(.)\1{4,}")
    (reduce
      (fn
        [ks [_ s]]
        (->>
          (triples s)
          (filter #(and (> % (- idx 999)) (< % idx)))
          (apply conj ks))) kys)))

(defn collect-keys
  [s]
  (fn
    [[kys triples] idx]
    (let [oc (count kys)
          hsh (md5 (str s idx))
          triples (conj-triples-of triples idx hsh)
          kys (conj-kys-of kys triples idx hsh)]
      ;(if (> (count kys) oc)
      ;  (do
      ;    (println idx)
      ;    (println hsh)
      ;    (println triples)
      ;    (println kys)
      ;    (println (count kys))))
      [kys triples])))

(defn keys-64th-idx
  [s]
  (nth
    (->>
      (range)
      (reductions (collect-keys s) [#{} {}])
      (drop-while #(-> % first count (< 64)))
      first
      first
      sort) 63))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
