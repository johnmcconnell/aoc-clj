(ns day-12.core
  (:gen-class))

(defn ->ipt
  [l]
  (->>
    (clojure.string/split l #"[\s,]+(<->)?[\s,]*")
    (map read-string)))

(defn ipts
  []
  (->>
    "input.txt"
    slurp
    clojure.string/split-lines
    (map ->ipt)))

(defn ipts'
  []
  (->>
    ["0 <-> 2"
     "1 <-> 1"
     "2 <-> 0, 3, 4"
     "3 <-> 2, 4"
     "4 <-> 2, 3, 6"
     "5 <-> 6"
     "6 <-> 4, 5"]
    (map ->ipt)))

(defn neighbors
  [ins]
  (reduce
    (fn
      [h1 [id & cds]]
      (reduce
        (fn
          [h2 cd]
          (update
            h2
            cd
            (fnil
              #(conj % id)
              #{id})))
        (update
          h1
          id
          (fnil
            #(apply conj % cds)
            (apply conj #{} cds)))
        cds))
    {}
    ins))

(defn find-class
  [nbrs seen st]
  (loop [s seen
         [id & others] st]
    (if (nil? id)
      s
      (let [nbs (get nbrs id)
            nxt (clojure.set/difference nbs s)]
        (recur
          (apply conj s id nxt)
          (apply conj others nxt))))))

(defn find-classes
  [nbrs]
  (let [remaining (->>
                    (vals nbrs)
                    (apply concat (keys nbrs))
                    (apply conj #{}))]
    (loop [r remaining
           grps []]
      (prn (count grps))
      (if (nil? (first r))
        grps
        (let [grp (find-class nbrs #{} [(first r)])]
          (recur
            (clojure.set/difference r grp)
            (conj grps grp)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
