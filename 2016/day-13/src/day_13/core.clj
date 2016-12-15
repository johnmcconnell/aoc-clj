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

(defn breadth-first-iter
  [{:keys [distances d seed pos end] :as state}]
  (if (= pos end)
    (assoc state :done true)
    (assoc state :done true)))
    ;(let [nxts (next-valid-positions seed distances)]))

(defn mindist
  [seed pos end]
  (let [distances (->>
                    (iterate
                      breadth-first-iter
                      {:distances {pos 0}
                       :d 0
                       :seed seed
                       :pos pos
                       :end end
                       :done false})
                    (drop-while #(not (% :done)))
                    first
                    :distances)]
    (distances end)))

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
