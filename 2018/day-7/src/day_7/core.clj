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

(defn assoc-remaining-work
  [d c]
  (println d c)
  (let [v (-> c first int (- 4))]
    (assoc d c v)))

(defn count-seconds
  [ttl rw-count
   finished working
   deps deps-pars
   ready]
  (println "==== rw-count ====")
  (println rw-count)
  (println "==== deps ====")
  (clojure.pprint/pprint deps)
  (println "==== working ====")
  (println working)
  (println "==== finished ====")
  (prn finished)
  (println "==== ready ====")
  (println ready)
  (if (and (empty? ready) (empty? working))
    ttl
    (let [dt (apply min (map second working))
          [w' df'] (reduce
                     (fn [[d f] [k v]]
                       (if (zero? (- v dt))
                         [(dissoc d k) (conj f k)]
                         [(assoc d k (- v dt)) f]))
                     [{} #{}]
                     working)
          f' (apply conj finished df')
          parent-groups (map #(vector % (deps-pars %)) df')
          deps' (reduce
                  (fn [dps1 [c pars]]
                    (reduce
                      #(update %1 %2 disj c)
                      dps1
                      pars))
                  deps
                  parent-groups)
          ready' (->>
                   (map second parent-groups)
                   (mapcat vec)
                   (filter (comp empty? deps')))
          ready'' (->>
                    ready'
                    (filter (comp not (partial contains? finished)))
                    (filter (comp not (partial contains? (set ready)))))
          rw-count' (+ rw-count (count df'))
          [nws ready4] (split-at rw-count' ready')
          _ (println "ready':" ready')
          _ (println "ready'':" ready'')
          w'' (reduce
                assoc-remaining-work
                w'
                nws)]
      (recur
        (+ ttl dt)
        rw-count'
        f'
        w''
        deps'
        deps-pars
        ready4))))

(defn part-2
  []
  (let [all-chars (set (concat (keys d-parents) (keys dependencies)))
        rdy (clojure.set/difference all-chars (-> dependencies keys set))
        w-cnt 5
       [working rdy'] (split-at w-cnt rdy)
        v (count-seconds
            0
            (- w-cnt (count working))
            #{}
            (reduce
              assoc-remaining-work
              {}
              working)
            dependencies
            d-parents
            rdy')]
    v))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
