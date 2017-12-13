(ns day-13.core
  (:gen-class))

(def ipt
  (->>
    "input.txt"
    slurp))

(defn ->scanners
  [t]
  (let [words (->>
                t
                clojure.string/split-lines
                (map #(clojure.string/split % #"[:\s]+"))
                (map (partial map read-string)))
        mx (apply max (map first words))
        v (vec (map (constantly [-1 -1]) (range 0 mx)))]
    (reduce
      (fn
        [v' [i l]]
        (assoc v' i [0 (* (dec l) 2)]))
      v
      words)))

(def scanners
  (->scanners ipt))

(defn ->scanners'
  [t]
  (->>
    t
    clojure.string/split-lines
    (map #(clojure.string/split % #"[:\s]+"))
    (mapv (partial mapv read-string))
    (mapv #(update % 1 (comp (partial * 2) dec)))))

(def my-srs
  (->scanners' ipt))

(defn scan++
  [coll]
  (map
    (fn
      [[idx l]]
      (if (= l -1)
        [idx l]
        [(-> idx inc (mod l)) l]))
    coll))

(defn penalty'
  [srs i]
  (let [[sidx l] (nth srs i)]
    (cond
      (= l -1) 0
      (= sidx 0) (* i (-> l (quot 2) inc))
      :else 0)))

(defn penalty''
  [srs]
  (prn srs)
  (reduce
    (fn
      [[ttl srs] i]
      [(+ (penalty' srs i) ttl) (scan++ srs)])
    [0 srs]
    (range 0 (count srs))))

(defn penalty'''
  [srs i]
  (let [[sidx l] (nth srs i)]
    (cond
      (= l -1) 0
      (= sidx 0) 1
      :else 0)))

(defn penalty''
  [srs]
  (reduce
    (fn
      [[ttl srs] i]
      [(+ (penalty''' srs i) ttl) (scan++ srs)])
    [0 srs]
    (range 0 (count srs))))

(defn penalty3
  [l d j]
  (if (= (mod (+ j d) l) 0)
    1
    0))

(defn penalty2
  [srs j]
  (reduce
    (fn
      [ttl [d l]]
      (+ (penalty3 l d j) ttl))
    0
    srs))

(defn tap
  [x]
  (prn x)
  x)

(defn min-delay
  [srs]
  (->>
    [srs 0]
    (iterate
      (fn
        [[s i _]]
        (->>
          s
          penalty''
          first
          (vector (scan++ s) (inc i)))))
    (drop-while (fn [[x i p]] (do (prn i p) (not= p 0))))
    first
    second))

(defn min-delay-3
  [srs]
  (->>
    [0 1]
    (iterate
      (fn
        [[i _]]
        (when (= (mod i 1000) 0) (prn i))
        (->>
          (penalty2 (vec srs) i)
          (vector (inc i)))))
    (drop-while (fn [[i p]] (not= p 0)))
    first
    first
    dec))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
