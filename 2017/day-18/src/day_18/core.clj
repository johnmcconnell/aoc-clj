(ns day-18.core
  (:gen-class))

(def ls-1
  (->>
    "input.txt"
    slurp
    clojure.string/split-lines))

(def ls-2
  ["set a 1"
   "add a 2"
   "mul a a"
   "mod a 5"
   "snd a"
   "set a 0"
   "rcv a"
   "jgz a -1"
   "set a 1"
   "jgz a -2"])

(defn rslv
  [d k]
  (if (symbol? k)
    (get d k 0)
    k))

(defn parse-ln
  [l]
  (let [[i x y] (clojure.string/split l #"\s+")
        [x' y'] (map (fnil read-string "nil") [x y])
        ->op (fn
               [x y op]
               (fn
                 [d]
                 (update d x (fnil #(op %1 (rslv d y)) 0))))]
    [(str i " " x " " y)
     (condp = i
       "snd" (fn
               [d]
               (assoc d :snd (rslv d x')))
       "set" (fn
               [d]
               (assoc d x' (rslv d y')))
       "add" (->op x' y' +)
       "mul" (->op x' y' *)
       "mod" (->op x' y' mod)
       "rcv" (fn
               [d]
               (if (= (rslv d x') 0)
                 d
                 (do (prn (get d :snd)) (assoc d :rcv (get d :snd)))))
       "jgz" (fn
               [{:keys [i] :as d}]
               (if (> (rslv d x') 0)
                 (update d :i + (dec (rslv d y')))
                 d)))]))

(def istrs-1
  (->>
    ls-1
    (map parse-ln)
    vec))

(def istrs-2
  (->>
    ls-2
    (map parse-ln)
    vec))

(def jmp-cnt (atom 0))

(defn parse-ln'
  [l]
  (let [[i x y] (clojure.string/split l #"\s+")
        [x' y'] (map (fnil read-string "nil") [x y])
        ->op (fn
               [x y op]
               (fn
                 [p1 p2]
                 [(update p1 x (fnil #(op %1 (rslv p1 y)) 0))
                  p2]))]
    [(str i " " x " " y)
     (condp = i
       "snd" (fn
               [p1 p2]
               (if (= (get p1 :p) 1)
                 (swap! jmp-cnt inc))
               [p1
                (->
                  p2
                  (update :queue (fnil conj []) (rslv p1 x'))
                  (assoc :locked false))])
       "set" (fn
               [p1 p2]
               [(assoc p1 x' (rslv p1 y'))
                p2])
       "add" (->op x' y' +)
       "mul" (->op x' y' *)
       "mod" (->op x' y' mod)
       "rcv" (fn
               [{:keys [queue] :as p1} p2]
               (let [[v & vs] queue]
                 (if (nil? v)
                   [(assoc p1 :locked true) p2]
                   [(assoc p1 x' v :queue (vec vs)) p2])))
       "jgz" (fn
               [{:keys [i] :as p1} p2]
               (if (> (rslv p1 x') 0)
                 [(update p1 :i + (dec (rslv p1 y'))) p2]
                 [p1 p2])))]))

(def istrs-1'
  (->>
    ls-1
    (map parse-ln')
    vec))

(def istrs-2'
  (->>
    ["snd 1"
     "snd 2"
     "snd p"
     "rcv a"
     "rcv b"
     "rcv c"
     "rcv d"]
    (map parse-ln')
    vec))

(defn simulate
  [d istrs]
  (let [i (get d :i 0)
        l (count istrs)]
    (if (or (< i 0) (>= i l))
      d
      (let [[s istr] (get istrs i)
            d' (->
                 d
                 istr
                 (update :i (fnil inc 0)))]
        (recur
          d'
          istrs)))))

(def pgrms [{'p 0 :p 0} {'p 1 :p 1}])

(defn simulate'
  [[p1 p2] istrs]
  (let [i (get p1 :i 0)
        l (count istrs)]
    (if (or (< i 0) (>= i l)
            (and (:locked p1) (:locked p2)))
      [p1 p2]
      (let [[s istr] (get istrs i)
            [p1' p2'] (->
                       p1
                       (istr p2))
            n (if (:locked p1')
                [p2' p1']
                [(update p1' :i (fnil inc 0)) p2'])]
        ;(prn (-> p1 :queue count) (-> p2 :queue count) s)
        (recur
          n
          istrs)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
