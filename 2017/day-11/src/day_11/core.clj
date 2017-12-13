(ns day-11.core
  (:gen-class))

(def ks [:n :nw :ne :s :sw :se])

(def instrs
  (->
    "input.txt"
    slurp
    (clojure.string/split #"[\s,]+")))

(defn cnts
  []
  (->>
    instrs
    (map keyword)
    frequencies))

(def my-reductions
  [{:n 1 :ne -1} ;{:nw 1}
   {:n 1 :nw -1} ;{:ne 1}
   {:nw 1 :ne 1}]) ;{:n 1}])

(defn ->dcoord
  [coord]
  (reduce
    (fn
      [h k]
      (update h k (fnil identity 0)))
    coord
    ks))

(defn abs [n] (max n (- n)))

(defn dist
  [& xs]
  (->>
    xs
    (map abs)
    (apply +)))

(defn min-coord
  [{:keys [n nw ne s sw se]}]
  (let [n' (- n s)
        nw' (- nw se)
        ne' (- ne sw)]
    (->>
      my-reductions
      (reduce
      (fn
        [[n' nw' ne'] {:keys [n nw ne]}]
        ;(prn n' nw' ne')
        ;(prn n nw ne)
        ;(prn (dist n' nw' ne'))
        (cond
          (and (nil? n) (pos? (* nw' ne')))
          (let [l (quot (+ nw' ne') 2)]
            [(+ n' l) (- nw' l) (- ne' l)])
          (and (nil? nw) (neg? (* n' ne')))
          (let [l (min (abs n') (abs ne'))]
            (if (>= n' 0)
              [(- n' l) (+ nw' l) (+ ne' l)]
              [(+ n' l) (- nw' l) (- ne' l)]))
          (and (nil? ne) (neg? (* n' nw')))
          (let [l (min (abs n') (abs nw'))]
            (if (>= n' 0)
              [(- n' l) (+ nw' l) (+ ne' l)]
              [(+ n' l) (- nw' l) (- ne' l)]))
          :else [n' nw' ne']))
      [n' nw' ne']))))

(defn min-cd
  [cd]
  (let [[n nw ne] (-> cd ->dcoord min-coord)
        e {:n n :nw nw :ne ne}]
    (if (= e cd)
      e
      (min-cd e))))

(defn run-it
  [its]
  (->>
    its
    (map keyword)
    (reductions
      (fn [fqs k]
        (update fqs k (fnil inc 0)))
      {})
    (map min-cd)
    (map #(apply dist (vals %)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
