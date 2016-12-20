(ns day-14.core
	(:import java.security.MessageDigest
           java.math.BigInteger)
  (:gen-class))

(defn md5
  ([s] (md5 s 0))
  ([s _]
   (let [algorithm (MessageDigest/getInstance "MD5")
         size (* 2 (.getDigestLength algorithm))
         raw (.digest algorithm (.getBytes s))
         sig (.toString (BigInteger. 1 raw) 16)
         padding (apply str (repeat (- size (count sig)) "0"))]
     (str padding sig))))

(def md5-m (memoize md5))

(defn norm-hash
  [s i]
  (md5-m (str s i)))

(defn stretch-hash
  [s i]
  (->>
    (range 0 2016)
    (reduce
      (fn
        [h _] (md5 h)) (md5 (str s i)))))

(def stretch-hash-m (memoize stretch-hash))

(defn conj-triples-of
  [triples idx hsh]
  (let [[m s] (->>
                hsh (re-find #"(\w)\1{2,}"))]
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
  [s f]
  (fn
    [[kys triples] idx]
    (let [oc (count kys)
          hsh (f s idx)
          triples (conj-triples-of triples idx hsh)
          kys (conj-kys-of kys triples idx hsh)]
      (if (= (mod idx 1000) 0)
        (do
          (println idx)
          (println (count kys))))
      [kys triples])))

(defn keys-64th-idx
  [s]
  (nth
    (->>
      (range)
      (reductions (collect-keys s norm-hash) [#{} {}])
      (drop-while #(-> % first count (< 64)))
      first
      first
      sort) 63))

(defn keys-2-64th-idx
  [s]
  (nth
    (->>
      (range)
      (reductions (collect-keys s stretch-hash) [#{} {}])
      (drop-while #(-> % first count (< 64)))
      first
      first
      sort) 63))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    "ahsbgdzn"
    keys-2-64th-idx
    println))
