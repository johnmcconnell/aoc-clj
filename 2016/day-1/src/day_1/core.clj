(ns day-1.core
  (:gen-class))

(def direction->idx
  {:north 0
   :east 1
   :south 2
   :west 3})

(def idx->direction (clojure.set/map-invert direction->idx))

(defn abs
  [x]
  (max x (- x)))

(defn parse-cmd
  [cmd]
  (let [turn-str (subs cmd 0 1)
        turn (if (= turn-str "L") :left :right)
        count (-> (subs cmd 1) Integer.)]
    {:turn turn
     :count count}))

(defn next-direction
  [direction turn]
  (let [inc-f (if (= turn :left) dec inc)
        idx (direction->idx direction)
        next-idx (mod (inc-f idx) 4)]
    (idx->direction next-idx)))

(defn step
  [{x :x y :y prev-direction :direction :as state} {turn :turn cnt :count}]
  (let [direction (next-direction prev-direction turn)]
    (-> (cond
          (= direction :north) (assoc state :y (+ y cnt))
          (= direction :south) (assoc state :y (- y cnt))
          (= direction :east) (assoc state :x (+ x cnt))
          (= direction :west) (assoc state :x (- x cnt)))
        (assoc :direction direction :turn turn))))

(defn delta-blocks
  [& inputs]
  (let [init-state {:x 0 :y 0 :direction :north}
        cmds (map parse-cmd inputs)
        {x :x y :y} (reduce step init-state cmds)]
    (+ (abs x) (abs y))))

(defn record-freq
  [freqs {x :x y :y}]
  (let [v {:x x :y y}
        prev (freqs v)]
    (if-not (nil? prev)
      {:done true :x x :y y}
      (assoc freqs v 1))))

(defn first-crossed
  [visited]
  (let [with-history (reductions record-freq {} visited)
        first-repeat (take-while #(not (% :done)) with-history)]
    (nth with-history (count first-repeat))))

(defn record-walk-y
  [x py y]
  (if (< py y)
    (map (fn [ny] {:x x :y ny}) (range py y))
    (map (fn [ny] {:x x :y ny}) (range py y -1))))

(defn record-walk-x
  [y px x]
  (if (< px x)
    (map (fn [nx] {:x nx :y y}) (range px x))
    (map (fn [nx] {:x nx :y y}) (range px x -1))))

(defn record-walk
  [[{px :x py :y} {x :x y :y}]]
  (if (= px x)
    (record-walk-y x py y)
    (record-walk-x y px x)))

(defn *delta-blocks
  [& inputs]
  (let [init-state {:x 0 :y 0 :direction :north}
        cmds (map parse-cmd inputs)
        reduxes (reductions step init-state cmds)
        visited (apply concat (map record-walk (partition 2 1 reduxes)))
        {x :x y :y} (first-crossed visited)]
    (+ (abs x) (abs y))))

(defn parse-inputs
  [input]
  (clojure.string/split input #", "))

(defn -main
  [& args]
  (->> args
      first
      parse-inputs
      (apply *delta-blocks)
      println))
