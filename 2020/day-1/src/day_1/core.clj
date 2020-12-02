(ns day-1.core
  (:gen-class))

(defn read-input
  []
  (->>
   "day-1.a.input"
   slurp
   clojure.string/split-lines
   (map read-string)))

(defn find-2020-nums
  [nums]
  (reduce
   (fn [seen n]
     (let [find (- 2020 n)]
       (if (contains? seen find)
         (reduced [n find])
         (conj seen n))))
   #{}
   nums))

(defn solve-a
  []
  (let [nums (read-input)
        [x y](find-2020-nums nums)]
    (* x y)))


(defn find-3-sum-nums
  [nums sum coll]
  (if (= (count coll) 3)
    (if (zero? sum)
      coll
      nil)
    (reduce
     (fn [nums* x]
       (let [nums* (disj nums* x)
             r (find-3-sum-nums
                nums*
                (- sum x)
                (conj coll x))]
         (if (empty? r)
           nums*
           (reduced r))))
     nums
     nums)))

(defn solve-b
  []
  (let [nums (set (read-input))
        [x y z] (find-3-sum-nums (set (read-input)) 2020 [])]
    (* x y z)))

(solve-b)
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
