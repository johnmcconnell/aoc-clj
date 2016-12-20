(ns day-14.core
	(:import java.security.MessageDigest
           java.math.BigInteger)
  (:gen-class))


(defn inf-1
  []
  (drop 1 (range)))

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
  (map #(md5 (str s %)) (inf-1)))

(defn conj-triples-of
  [triples idx hsh]
  (let [s (->>
            hsh
            (re-find #"(.)\1{2,}")
            second)]
    (update-in triples [s] #(conj % idx))))

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
          (filter #(> % (- idx 1000)))
          (map (fn [_] [s idx]))
          (apply conj ks))) kys)))

(defn collect-keys
  [s]
  (fn
    [[kys triples] idx]
    (let [hsh (md5 (str s idx))
          triples (conj-triples-of triples idx hsh)
          kys (conj-kys-of kys triples idx hsh)]
      (println (count kys))
      (println idx)
      [kys triples])))

(defn keys-64th-idx
  [s]
  (->>
    (inf-1)
    (reductions (collect-keys s) [[] {}])
    (drop-while #(-> % first count (< 64)))
    first
    first
    last
    second))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
