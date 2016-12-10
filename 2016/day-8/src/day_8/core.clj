(ns day-8.core
  (:gen-class))

(defn fill-mx
  [v r-cnt c-cnt]
  (->
    v
    constantly
    (map (range 0 c-cnt))
    constantly
    (map (range 0 r-cnt))))

(defn ->vec
  [mx]
  (->>
    mx
    (map vec)
    vec))


(defn turn-on-with
  [mx [x y v]]
  (->
    mx
    ->vec
    (update-in [x y] (constantly v))))

(defn turn-on
  [mx [x y]]
  (turn-on-with mx [x y 1]))

(defn merge-mx
  [mx s-r-cnt s-c-cnt]
  (let [r-cnt (min (count mx) s-r-cnt)
        c-cnt (min (count (first mx)) s-c-cnt)]
    (->>
      (for [x (range 0 r-cnt)
            y (range 0 c-cnt)]
        [x y])
      (reduce turn-on mx))))

(defn rotate-row
  [mx idx step]
  (let [nl (->vec mx)
        row-size (count (first mx))
        row (->
              nl
              (nth idx)
              cycle)
        shifted (->>
                 row
                 (drop (mod (- step) row-size))
                 (take row-size))]
    (assoc nl idx shifted)))

(defn col-pos-val
  [mx c-idx step]
  (fn
    [r-idx]
    (let [col-size (count mx)
          next-r-idx (mod (- r-idx step) col-size)
          v (->
              mx
              ->vec
              (get-in [next-r-idx c-idx]))]
          [r-idx c-idx v])))

(defn rotate-col
  [mx idx step]
  (->>
    mx
    count
    (range 0)
    (map (col-pos-val mx idx step))
    (reduce turn-on-with mx)))

(def rect merge-mx)

(defn instr->
  [[fst snd _ fth] k]
  (condp = k
    :r (-> (clojure.string/split fst #"x") (nth 1) Integer.)
    :c (-> (clojure.string/split fst #"x") first Integer.)
    :idx (-> (clojure.string/split snd #"=") (nth 1) Integer.)
    :step (Integer. fth)))

(defn instr-dispatch
  [mx s]
  (let [parts (clojure.string/split s #"\s+")
        cmd (first parts)
        args (rest parts)]
    ;(println mx cmd args)
    (condp = cmd
      "rect" (rect mx (instr-> args :r) (instr-> args :c))
      "rotate" (condp = (first args)
                 "row"
                 (rotate-row mx (instr-> args :idx) (instr-> args :step))
                 "column"
                 (rotate-col mx (instr-> args :idx) (instr-> args :step))))))

(defn clear-it
  [x]
  (condp = x
    0 " "
    1 "*"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [mx (->>
             *in*
             slurp
             clojure.string/split-lines
             (reduce
               instr-dispatch (fill-mx 0 6 50)))]
    (doseq [line mx]
      (println (apply str (map clear-it line))))))
