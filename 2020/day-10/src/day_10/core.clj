(ns day-10.core
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
    (read-string l)))

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

(defn valid-neighbors
  [coll i ln]
  (if (and (<= 0 i) (< i ln))
    (let [v (nth coll i)]
      (->>
       (range (inc i) (min (+ i 4) ln))
       (filter #(<= (- (nth coll %) v) 3))))
    nil))

(defn find-distinctions
  ([coll]
   (find-distinctions {} coll 0 (count coll)))
  ([cache coll i ln]
   (cond
     (contains? cache i) cache
     (= i (dec ln)) (assoc cache i 1)
     :else (let [nbs (valid-neighbors coll i ln)
                 cache (reduce #(find-distinctions %1 coll %2 ln) cache nbs)
                 ttl (reduce #(+ %1 (get cache %2)) 0 nbs)]
             (assoc cache i ttl)))))

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
     sort
     vec))
  a-input
  (find-jolt-diffs a-input)
  (* 68 34)

  c-input
  (find-jolt-diffs c-input)

  (find-distinctions (into [] (cons 0 a-input)))
  
  (def rs (find-distinctions c-input))
  (->>
   rs
   keys
   sort
   (mapv #(get rs %1))
   )

  (+ 10976 5488 2744)
  (- 19208 )

  c-input
  d-input
  (def c-input
    (->
     (read-input-lines
      {::part :c
       ::parse parse-in-line})
     sort
     vec))

  (def d-input
    (->
     (read-input-lines
      {::part :d
       ::parse parse-in-line})
     sort
     vec))

  (def my-p
    {::instructions (vec a-input)
     ::i 0
     ::accumulator 0})

  (def d-input
    (read-input-lines
     {::part :d
      ::parse parse-in-line}))
  )
