(ns day-8.core
  (:gen-class))

(def ls
  (->>
    (slurp "input.txt")
    (clojure.string/split-lines)))

(defn g-instrs
  [lines]
  (for [l lines]
    (let [[r1' f' a' _ r2' op' b'] (clojure.string/split l #"\s+")
          f (condp = f'
                 "inc" +
                 "dec" -)
          op (condp = op'
               "==" =
               "!=" not=
               (-> op' read-string resolve))
          _ (if (nil? op) (prn op'))
          a (-> a' read-string)
          b (-> b' read-string)
          r1 (keyword r1')
          r2 (keyword r2')]
      [r1 f a r2 op b])))

(def ss
  ["b inc 5 if a > 1"
   "a inc 1 if b < 5"
   "c dec -10 if a >= 1"
   "c inc -20 if c == 10"])

(defn run
  [cmds]
  (reduce
    (fn
      [s [r1 f a r2 op b]]
      (let [rv1 (get s r1 0)
            rv2 (get s r2 0)]
        (if (op rv2 b)
          (assoc s r1 (f rv1 a))
          s)))
    {}
    cmds))

(defn run-max
  [cmds]
  (reduce
    (fn
      [[mv s] [r1 f a r2 op b]]
      (let [rv1 (get s r1 0)
            rv2 (get s r2 0)]
        (if (op rv2 b)
          (let [v (f rv1 a)
                s' (assoc s r1 v)]
            (if (> v mv)
              [v s']
              [mv s']))
          [mv s])))
    [-1 {}]
    cmds))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
