(ns day-6.core
  (:gen-class))

(defn get-s [] (slurp "input.txt"))
(defn get-s-lns [] (-> (get-s) (clojure.string/split-lines)))
(defn get-s-wrds [] (-> (get-s) (clojure.string/split #"\s+")))

(def banks (->> (get-s-wrds) (map read-string) vec))

(defn find-max
  [banks]
  (->>
    banks
    (map-indexed vector)
    (reduce
      (fn [[mi mc] [i c]]
        (cond
          (nil? mc) [i c]
          (> c mc) [i c]
          :else [mi mc]))
      [-1 nil])))

(defn cycle-inc
  [banks]
  (let [l (count banks)]
    (fn
      [i]
      (mod (inc i) l))))

(defn distribute
  [banks]
  (let [[i c] (find-max banks)
        c-inc (cycle-inc banks)]
    (loop [x 0
           bks (assoc banks i 0)
           idx i]
      (if (= x c)
        bks
        (let [idx' (c-inc idx)]
          (recur
            (inc x)
            (update bks idx' inc)
            idx'))))))

(defn find-repeat-cnt
  [banks]
  (loop [cnt 1
         states #{banks}
         bks banks]
    (let [bks' (distribute bks)]
      (if (contains? states bks')
        cnt
        (recur
          (inc cnt)
          (conj states bks')
          bks')))))

(defn find-repeat-loop-cnt
  [banks]
  (loop [cnt 1
         states #{banks}
         cnts {banks cnt}
         bks banks]
    (let [bks' (distribute bks)
          cnt' (inc cnt)]
      (if (contains? states bks')
        (- cnt' (get cnts bks'))
        (recur
          cnt'
          (conj states bks')
          (assoc cnts bks' cnt')
          bks')))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
