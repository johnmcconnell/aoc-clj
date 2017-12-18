(ns day-17.core
  (:gen-class))

(def j 304)

(defn ci
  [i l]
  (mod i l))

(defn ->lm
  [coll]
  (->>
    (concat [(last coll)] coll [(first coll)])
    (partition 3 1)
    (reduce
      (fn
        [d [pv v nv]]
        (->
          d
          (assoc v [pv nv])))
      {})))

(defn i-after
  [d v v']
  (let [[pv nv] (get d v)]
    (-> d
        (assoc v [pv v'])
        (assoc v' [v nv])
        (update nv #(assoc % 0 v')))))

(defn nth-of
  [d v' s']
  (loop [s s'
         v v']
    (if (= s 0)
      v
      (recur
        (dec s)
        (-> v d second)))))

(defn ->coll
  ([d] (->coll d #{} (-> d keys first) []))
  ([d ks k coll]
   (if (or (nil? k) (contains? ks k))
     coll
     (recur
       d
       (conj ks k)
       (-> k d second)
       (concat coll [k])))))

(defn build
  [s t]
  (->>
    (range 1 (inc t))
    (reduce
      (fn
        [[d v] i]
        (let [v' (nth-of d v s)
              d' (i-after d v' i)]
          (if (= (mod i 10000) 0) (prn i))
          ;(prn v d)
          ;(prn v' d')
          [d' i]))
      [(->lm [0]) 0])
    first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
