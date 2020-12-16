(ns day-15.core
  (:gen-class))

(defn valid-num?
  [mn mx]
  (fn [v]
    (let [v* (read-string v)]
      (and
       (<= mn v*)
       (<= v* mx)))))

(defn valid-reg?
  [re]
  (fn [s]
    (re-matches re s)))

(defn read-input
  [{::keys [part
            raw]
    :or {part :a}}]
  (if (nil? raw)
    (let [f-name (str (name part) ".input")]
      (-> f-name slurp))
    raw))

(defn read-input-lines
  [{::keys [parse]
    :or {parse identity}
    :as ctx}]
  (->>
   (read-input ctx)
   clojure.string/split-lines
   (map parse)))

(defn parse-in-line
  [l]
  (if (empty? l)
    nil
    (let [xs (clojure.string/split l #",")]
      (mapv read-string xs))))

(defn reg-deps
  [dep-coll]
  (reduce
   (fn [rdeps [h deps]]
     (assoc rdeps (:id h) deps))
   {}
   dep-coll))

(defn reverse-deps
  [dep-coll]
  (reduce
   (fn [rdeps [h deps]]
     (reduce #(update %1 (:id %2) (fnil conj #{}) (:id h)) rdeps deps))
   {}
   dep-coll))

(defn find-all-parents
  [r-deps node]
  (loop [queue [node]
         seen #{}]
    (if (empty? queue)
      seen
      (let [[node & queue] queue
            deps (->>
                  (get r-deps node)
                  (remove #(contains? seen %1)))]
        (recur
         (concat queue deps)
         (apply conj seen deps))))))

(defn resolve-deps
  [bag-deps cache {:keys [id count]}]
  (if (contains? cache id)
    cache
    (let [deps (get bag-deps id)
          cache (reduce
                 (fn [c1 dep]
                   (let [c2 (resolve-deps bag-deps c1 dep)]
                     (merge c1 c2)))
                 cache
                 deps)

          v (->
             (mapcat
              (fn [{:keys [count id] :as d}]
                (let [r-coll (->>
                              (get cache id)
                              (mapv #(update % :count * count)))]
                  (conj r-coll d)))
              deps)
             vec)]
      (assoc
       cache
       id
       v))))

(defn eval-program-and-prevent-loop
  [{::keys [instructions i accumulator]
    :as ctx}
   seen]
  (cond
    (<= (count instructions) i) (assoc ctx ::halted? true)
    (contains? seen i) (assoc ctx ::halted? false)
    :else (let [[instr x] (get instructions i)
                seen* (conj seen i)
                ctx* (case instr
                       :nop (update ctx ::i inc)
                       :acc (-> ctx (update ::i inc) (update ::accumulator + x))
                       :jmp (update ctx ::i + x))]
            (recur ctx* seen*))))

(defn find-program
  [{::keys [instructions]
    :as ctx}]
  (let [possible-ctxs (for [[i [op x]] (map-indexed vector instructions)
                            :when (contains? #{:nop :jmp} op)]
                        (let [instr* [(get {:nop :jmp :jmp :nop} op) x]
                              instrs (assoc instructions i instr*)]
                          (assoc ctx ::instructions instrs)))]
    (->>
     possible-ctxs
     (map #(eval-program-and-prevent-loop %1 #{}))
     (filter (comp ::halted?)))))

(defn find-contiguous
  [coll v]
  (loop [i 0
         j 0
         curr 0]
    (if (or (>= j (count coll)))
      -1
      (cond
        (= curr v) (subvec coll i (inc j))
        (< curr v) (recur i (inc j) (+ curr (nth coll j)))
        (> curr v) (recur (inc i) j (- curr (nth coll i)))))))

(defn debug-state
  [state]
  (let [vls (keys state)
        [[x-mn x-mx]
         [y-mn y-mx]] (mapv
                       #(let [ps (mapv %1 vls)]
                          [(apply min ps) (apply max ps)])
                       [first second])]
    (for [x (range x-mn (inc x-mx))]
      (reduce
       (fn [s y]
         (let [v (get state [x y])
               c (cond
                   (= v :occupied) \#
                   (= v :empty) \L
                   (nil? v) \.)]
           (str s c)))
       ""
       (range y-mn (inc y-mx))))))

(defn find-first-nbr
  [state
   s
   [[x-mn x-mx]
    [y-mn y-mx]]
   [x y]
   [dx dy]]
  (loop [i (+ x dx)
         j (+ y dy)]
    (if (and
         (<= x-mn i) (<= i x-mx)
         (<= y-mn j) (<= j y-mx))
      (let [v (get state [i j])]
        (if (= v :empty)
          (conj s [i j])
          (recur (+ i dx) (+ j dy))))
      s)))

(defn chair-deps
  [state]
  (let [vls (keys state)
        constraints (mapv
                     #(let [ps (mapv %1 vls)]
                        [(apply min ps) (apply max ps)])
                     [first second])]
    (reduce
     (fn [deps [[x y] _]]
     (let [dirs (for [dx (range -1 2)
                      dy (range -1 2)
                      :when (or (not= dx 0) (not= dy 0))]
                  [dx dy])
           nbs (reduce
                #(find-first-nbr state %1 constraints [x y] %2)
                #{}
                dirs)]
       (assoc deps [x y] nbs)))
   {}
   state)))

(defn chair-step-2
  [state deps]
  (let [next-val (fn [s [x y] v]
                   (let [nbs (->>
                              (get deps [x y])
                              (mapv #(get state %1))
                              frequencies)]
                     (cond
                       (and (= v :empty) (zero? (get nbs :occupied 0))) :occupied
                       (and (= v :occupied) (>= (get nbs :occupied 0) 5)) :empty
                       :else v)))]
    (reduce
     (fn [s [pos v]]
       (assoc s pos (next-val state pos v)))
     {}
     state)))

(defn find-stable-2
  [state deps]
  (loop [s state]
    (let [n (chair-step-2 s deps)]
      (if (= n s)
        n
        (recur n)))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

(defn abs
  [x]
  (/ (* x x) x))

(defn sieve-chinese-remainder
  [n a]
  (let [reducer (fn [[x p] [n a]]
                  (loop [i 0]
                    (let [v (+ x (* i p))]
                      (if (= (mod v n) a)
                        [v (* n p)]
                        (recur (inc i))))))
        t-period (reduce reducer [0 1] (map vector n a))]
    (first t-period)))

(defn sync-busses
  [{::keys [busses]}]
  (let [[n m] (mapv #(mapv % busses) [second first])
        j (sieve-chinese-remainder n m)]
    j))

(defn exp [x n]
  (loop [acc 1
         n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn spoken-game
  [coll limit]
  (loop [i 0
         p-num nil
         numbers {}]
    (when (zero? (mod i 100000))
      (println i p-num))
    (if (= i limit)
      p-num
      (let [num (cond
                  (< i (count coll)) (nth coll i)
                  (contains? numbers p-num) (let [[j k] (get numbers p-num)]
                                              (if (nil? k)
                                                0
                                                (- j k)))
                  :else 0)]
        (recur
         (inc i)
         num
         (update
          numbers
          num (fn [[j k]]
                [i j])))))))

(comment
  (def a-input
    (let [part :a
          r (read-input-lines
             {::part :a
              ::parse parse-in-line})]
      r))

  (def c-input
    (let [part :c
          raw "0,3,6"
          r (read-input-lines
             {::part :c
              ::raw raw
              ::parse parse-in-line})]
      (first r)))
  (spoken-game c-input 10)
  (spoken-game [3 1 2] 30000000)
  (def my-f (spoken-game [0,8,15,2,12,1,4] 30000000))
  my-f

  (def d-input
    (let [part :d
          raw "hello"
          r (read-input-lines
             {::part :d
              ::raw raw
              ::parse parse-in-line})]
      r))

  (println)
  )
