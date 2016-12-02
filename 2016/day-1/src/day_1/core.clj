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

(defn parse-inputs
  [input]
  (clojure.string/split input #", "))

(defn -main
  [& args]
  (->> args
      first
      parse-inputs
      (apply delta-blocks)
      println))
