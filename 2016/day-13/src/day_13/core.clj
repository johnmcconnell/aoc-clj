(ns day-13.core
  (:gen-class))

(defn type-of-pos
  [x y seed]
  (->
    (+
     (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)
     seed)
    Long/bitCount
    even?
    (if :open :wall)))

(defn- st
  [f x]
  (max (f x 1) 0))

(defn next-positions
  [visited x y]
  (let [all-visited (conj visited [x y])]
    (->>
      [[(st - x) y] [(st + x) y]
       [x (st - y)] [x (st + y)]]
      (remove #(all-visited %))
      distinct
      set)))

(defn next-valid-positions
  [visited seed x y]
  (->>
    (next-positions visited x y)
    (remove
      (fn
        [[x y]]
        (=
         (type-of-pos x y seed) :wall)))
    set))

(defn conj-pts
  [{:keys [visited seed] :as state} [x y]]
  (let [nvps (next-valid-positions visited seed x y)]
    (->
      state
      (update :visited #(apply conj % nvps))
      (update :next-pts #(apply conj % nvps)))))

(defn breadth-first-iter
  [{:keys [visited d seed next-pts end] :as state}]
  (cond
    (some #{end} next-pts) (assoc state :done true)
    (= d 10000000) (assoc state :done true)
    :else (let [{next-pts :next-pts
                 visited :visited}
                (reduce
                  conj-pts
                  {:visited visited
                   :next-pts []
                   :seed seed}
                  next-pts)]
            (->
              state
              (assoc :next-pts next-pts)
              (assoc :visited visited)
              (update :d inc)))))

(defn mindist
  [seed pos end]
  (->>
    (iterate
      breadth-first-iter
      {:visited #{pos}
       :d 0
       :seed seed
       :next-pts [pos]
       :end end
       :done false})
    (drop-while #(not (% :done)))
    first
    :d))

(defn -main
  "I don't do a whole lot."
  [& args]
  (->
    (mindist 1350 [1 1] [31 39])
    println))
