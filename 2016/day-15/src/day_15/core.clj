(ns day-15.core
  (:gen-class))

(defn pos-0?
  [[{:keys [idx size]} ts]]
  (->
    idx
    (+ ts)
    (mod size)
    (= 0)))

(defn pass-through?
  [disks]
  (fn
    [t]
    (->>
      (range 0 (count disks))
      (map (partial + t))
      (map #(vec [%1 %2]) disks)
      (every? pos-0?))))

(defn first-pass
  [disks]
  (->>
    (range)
    (drop-while (complement (pass-through? disks)))
    first
    dec))

(def input
  [{:t 0 :idx 1 :size 17}
   {:t 0 :idx 0 :size 7}
   {:t 0 :idx 2 :size 19}
   {:t 0 :idx 0 :size 5}
   {:t 0 :idx 0 :size 3}
   {:t 0 :idx 5 :size 13}
   {:t 0 :idx 0 :size 11}])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    input
    first-pass
    println))
