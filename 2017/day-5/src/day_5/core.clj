(ns day-5.core
  (:gen-class))

(defn find-it
  [v']
  (let [l (count v')]
    (loop [cnt 0
           i 0
           v v']
      (if (or (< i 0) (>= i l))
        cnt
        (let [j (get v i)]
          (recur
            (inc cnt)
            (+ i j)
            (update v i inc)))))))

(defn find-it'
  [v']
  (let [l (count v')]
    (loop [cnt 0
           i 0
           v v']
      (if (or (< i 0) (>= i l))
        (do (prn v) cnt)
        (let [j (get v i)
              f (if (>= j 3) dec inc)]
          (recur
            (inc cnt)
            (+ i j)
            (update v i f)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
