(ns day-12.core
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
  [{::keys [part]
    :or {part :a}}]
  (let [f-name (str (name part) ".input")]
    (-> f-name slurp)))

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
    (let [[op & n] l
          n (read-string (apply str n))]
      (when (contains? #{\R \L} op)
        (assert (zero? (mod n 90))))
      {::op op
       ::v n})))

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

(defn find-jolt-diffs
  [coll]
  (->>
   (partition 2 1 coll)
   (mapv (fn [[x y]] (- y x)))
   frequencies))

(defn input->positions
  [coll]
  (->>
   (for [[i l] (map-indexed vector coll)
         [j c] (map-indexed vector l)
         :when (= c \L)]
     [i j])
   (reduce #(assoc %1 %2 :empty) {})))

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

(defn sim-ship
  [coll]
  (let [ln (count coll)
        directions [\W \N \E \S]
        i->dir (into {} (map-indexed vector directions))
        dir->i (clojure.set/map-invert i->dir)
        dir->mag {\N [0 1]
                  \E [1 0]
                  \S [0 -1]
                  \W [-1 0]}
        turn (fn [{::keys [dir] :as s}
                  {::keys [op v]}]
               (cond
                 (contains? #{\L \R} op) (let [m (get {\L -1 \R 1} op)
                                               d-dir (* m (/ v 90))
                                               i (get dir->i dir)
                                               j (mod (+ i d-dir) (count directions))]
                                           (get i->dir j))
                 :else dir))
        move (fn [{::keys [pos dir] :as s}
                  {::keys [op v]}]
               (let [[x y] pos
                     [dx dy] (cond
                               (contains? #{\L \R} op) [0 0]
                               (contains? #{\W \N \E \S} op) (dir->mag op)
                               :else (dir->mag dir))]
                 [(+ x (* v dx))
                  (+ y (* v dy))]))
        step (fn [{::keys [pos dir] :as s}
                  {::keys [op v] :as ist}]
               (->
                s
                (assoc ::dir (turn s ist))
                (assoc ::pos (move s ist))))]
    (loop [i 0
           s {::pos [0 0]
              ::dir \E}]
      (println s)
      (if (< i ln)
        (recur (inc i) (step s (nth coll i)))
        s))))

(defn sim-ship-2
  [coll]
  (let [ln (count coll)
        directions [\W \N \E \S]
        i->dir (into {} (map-indexed vector directions))
        dir->i (clojure.set/map-invert i->dir)
        dir->mag {\N [0 1]
                  \E [1 0]
                  \S [0 -1]
                  \W [-1 0]}
        turn (fn [{::keys [way-pos dir] :as s}
                  {::keys [op v]}]
               (cond
                 (contains? #{\L \R} op) (let [m (get {\L -1 \R 1} op)
                                               d-dir (* m (/ v 90))
                                               i (get dir->i dir)
                                               j (mod (+ i d-dir) (count directions))
                                               dir* (get i->dir j)
                                               n (get {\L 0 \R 1} op)
                                               way-pos* (reductions
                                                         (fn [[x y] _]
                                                           (update [y x] n * -1))
                                                         way-pos
                                                         (range 0 (Math/abs d-dir)))]
                                           {::dir dir*
                                            ::way-pos (last way-pos*)})
                 (contains? #{\W \N \E \S} op) (let [[x y] way-pos
                                                     [dx dy] (cond
                                                               (contains? #{\L \R} op) [0 0]
                                                               (contains? #{\W \N \E \S} op) (dir->mag op)
                                                               :else (dir->mag dir))
                                                     way-pos* [(+ x (* v dx))
                                                               (+ y (* v dy))]]
                                                 {::way-pos way-pos*})
                 :else nil))
        move (fn [{::keys [ship-pos
                           way-pos
                           dir] :as s}
                  {::keys [op v]}]
               (let [[x y] ship-pos
                     [dx dy] (cond
                               (contains? #{\L \R} op) [0 0]
                               (contains? #{\W \N \E \S} op) [0 0]
                               :else way-pos)]
                 [(+ x (* v dx))
                  (+ y (* v dy))]))
        step (fn [s
                  ist]
               (->
                s
                (merge (turn s ist))
                (assoc ::ship-pos (move s ist))))
        init-s {::ship-pos [0 0]
                ::way-pos [10 1]
                ::dir \E}]
    (println init-s)
    (loop [i 0
           s init-s]
      (if (< i ln)
        (let [n (nth coll i)
              i* (inc i)
              s* (step s n)]
          #_(do
            (println ">>>")
            (println n)
            (println s*)
            )
          (recur i* s*))
        s))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (def a-input
    (->
     (read-input-lines
      {::part :a
       ::parse parse-in-line})
     vec))

  (sim-ship-2 a-input)
  (+ 18300 56591)

  (+ 863 824)

  (+ 20658 215)
  a-input

  d-input
  (def c-input
    (->
     (read-input-lines
      {::part :c
       ::parse parse-in-line})
     vec))

  (def d-input
    (->
     (read-input-lines
      {::part :d
       ::parse parse-in-line})
     sort
     vec))

  (def d-input
    (read-input-lines
     {::part :d
      ::parse parse-in-line}))

  )
