(ns day-11.core
  (:gen-class))

(def finished-state
  {4 #{"L-G" "H-G" "E" "H-M" "L-M"}})

(defn finished?
  [state]
  true)

(defn elevator-floor
  [idx [k v]] (if (nil? idx) (if (some #{"E"} v) k nil) idx))

(defn next-floors
  [floor]
  (condp = floor
    1 #{2}
    4 #{3}
    #{(max (dec floor) 1) (min (inc floor) 4)}))

(defn conj-pair
  [coll idx-1]
  (fn
    [pairs idx-2]
    (conj pairs [(coll idx-1) (coll idx-2)])))

(defn conj-pairs
  [coll cnt]
  (fn
    [pairs idx]
    (reduce (conj-pair coll idx) pairs (range (inc idx) cnt))))

(defn pairs
  [coll]
  (let [cnt (count coll)]
    (if (< cnt 2)
      #{}
      (reduce (conj-pairs coll cnt) #{} (range 0 cnt)))))

(defn conj-gen-chip
  [[gens chips] item]
  (let [[id tag] (clojure.string/split item #"-")]
    (condp = tag
      nil [gens chips]
      "G" [(conj gens id) chips]
      "M" [gens (conj chips id)])))

(defn valid-floor?
  [[_ items]]
  (let [[gens chips] (reduce conj-gen-chip [#{} #{}] items)]
    (cond
      (= (count gens) 0) true
      :else (every? gens chips))))

(defn valid-state?
  [state]
  (every?
    valid-floor? state))

(defn conj-item
  [visited state curr-floor next-floor]
  (fn
    [states item]
    (let [next-state (->
                       state
                       (update-in [curr-floor] disj "E")
                       (update-in [next-floor] conj "E")
                       (update-in [curr-floor] disj item)
                       (update-in [next-floor] conj item))]
      (cond
        (visited next-state) states
        (valid-state? next-state) (conj states next-state)
        :else states))))

(defn conj-item-pair
  [visited state curr-floor next-floor]
  (fn
    [states [item1 item2]]
    (let [next-state (->
                       state
                       (update-in [curr-floor] disj "E")
                       (update-in [next-floor] conj "E")
                       (update-in [curr-floor] disj item1)
                       (update-in [next-floor] conj item1)
                       (update-in [curr-floor] disj item2)
                       (update-in [next-floor] conj item2))]
      (cond
        (visited next-state) states
        (valid-state? next-state) (conj states next-state)
        :else states))))

(defn conj-next-floor-states
  [visited state curr-floor]
  (fn [states next-floor]
    (let [curr-items (-> curr-floor state (disj "E"))
          curr-item-pairs (pairs (vec curr-items))
          next-items (state next-floor)
          movable-states (apply
                           conj
                           (reduce
                             (conj-item
                               visited state curr-floor next-floor)
                             #{}
                             curr-items)
                           (reduce
                             (conj-item-pair
                               visited state curr-floor next-floor)
                             #{}
                             curr-item-pairs))]
      (apply conj states movable-states))))

(defn next-valid-states
  [visited state]
  (let [curr-floor (reduce elevator-floor nil state)
        floors (next-floors curr-floor)]
    (reduce (conj-next-floor-states visited state curr-floor) #{} floors)))

(defn conj-next-states
  [[visited next-states] state]
  [(conj visited state)
   (apply conj next-states (next-valid-states visited state))])

(defn min-steps
  ([state] (min-steps #{} 0 [state]))
  ([visited d states]
   (cond
     (empty? states) -1
     (some finished? states) d
     :else (let [[visited states] (reduce conj-next-states [visited #{}] states)]
             (min-steps visited (inc d) states)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
