(ns day-7.core
  (:require
    [clojure.pprint]
    [clojure.set]
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i'
  (clojure.string/split-lines my-i))

(defn ->instruction
  [s]
  (let [pts (clojure.string/split s #"\s+")]
    [(nth pts 1) (nth pts 7)]))

(def instructions
  (map ->instruction my-i'))

(defn ->parents
  [s]
  (let [pts (clojure.string/split s #"\s+")]
    [(nth pts 1) (nth pts 7)]))

(def d-parents
  (reduce
    (fn
      [d [c p]]
      (update d c (fnil conj #{}) p))
    {}
    instructions))

(def dependencies
  (reduce
    (fn
      [d [c p]]
      (update d p (fnil conj #{}) c))
    {}
    instructions))

(defn char-range
  [start end]
  (map char (range (int start) (inc (int end)))))

(defn ->2dvec
  [x y]
  (vec (replicate x (vec (replicate y nil)))))

(defn abs
  [n]
  (max n (- n)))

(defn m-dist
  [x y]
  (as-> (map (comp abs -) x y) $
    (apply + $)))

(defn find-order
  [order deps dparents [r & rs]]
  (println "=== starting ===")
  (prn order r rs)
  #_(println "=== deps ===")
  (clojure.pprint/pprint deps)
  (if (nil? r)
    order
    (let [pars (get dparents r)
          deps' (reduce
                  #(update %1 %2 disj r)
                  deps
                  pars)
          ready' (->>
                   pars
                   (filter (comp empty? deps'))
                   set)
          ready'' (clojure.set/difference ready' (set (apply conj order r rs)))]
      #_(println "=== parent ===")
      #_(prn pars)
      #_(prn ready')
      #_(prn ready'')
      (recur
        (conj order r)
        deps'
        dparents
        (-> rs (concat ready'') sort)))))

(defn part-1
  []
  (let [all-chars (set (concat (keys d-parents) (keys dependencies)))
        rdy (clojure.set/difference all-chars (-> dependencies keys set))
        v (find-order
            []
            dependencies
            d-parents
            (sort rdy))]
    (apply str v)))

(defn count-seconds
  [ttl rw-count
   finished working
   deps dparents
   ready]
  nil)

(defn part-2
  []
  (let [all-chars (set (concat (keys d-parents) (keys dependencies)))
        rdy (clojure.set/difference all-chars (-> dependencies keys set))
        v (count-seconds
            dependencies
            d-parents
            (sort rdy))]
    (apply str v)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
