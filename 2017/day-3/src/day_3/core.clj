(ns day-3.core
  (:gen-class))

(defn layer-cnt
  [x]
  (if (= x 1)
    [0 x]
    (loop [cnt 9
           l 1]
      (let [l' (inc (* 2 l))]
        (if (<= x cnt)
          [l cnt]
          (recur (+ cnt 4 (* 4 l')) (inc l)))))))

(defn moves-of
  [l]
  (->>
    [(for [x (range 1 l)]
       [0 x])
     (for [x (range 1 l)]
       [(- x) (dec l)])
     (for [x (range 1 l)]
       [(- (dec l)) (dec (- l x))])
     (for [x (range 1 l)]
       [(inc (- x l)) 0])]
    (apply concat)))

(defn add-coords
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(def neighbor-offsets
  (for [x (range -1 2)
        y (range -1 2)
        :when (not= x y 0)]
    [x y]))

(defn fill-in
  [coord x]
  (fn
    [[nxt grid] move]
    (if nxt
      [nxt grid]
      (let [coord' (add-coords coord move)
            ->nbr (partial add-coords coord')
            nbrs (map ->nbr neighbor-offsets)
            cnt (reduce
                  (fn [s ncrd]
                    (+ s (get grid ncrd 0)))
                  0
                  nbrs)]
        (if (> cnt x)
          [cnt grid]
          [nxt (assoc grid coord' cnt)])))))

(defn next-larger
  [x]
  (if (= x 0)
    1
    (loop [grid {[0 0] 1}
           coord [1 -1]
           l 1]
      (let [l' (inc (* 2 l))
            moves (moves-of l')
            [nxt grid'] (reduce
                          (fill-in coord x)
                          [nil grid]
                          moves)]
        (if nxt
          nxt
          (recur
            grid'
            (add-coords coord [1 -1])
            (inc l)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
