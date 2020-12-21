(ns day-21.core
  (:require [clojure.pprint])
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
    (let [i (clojure.string/index-of l "(")
          ln (count l)
          off (+ i (count "contains") 2)
          [ings algs] [(subs l 0 (dec i)) (subs l off (dec ln))]
          [ings algs] (mapv (fn [[w r]] (into #{} (clojure.string/split w r))) [[ings #"\s+"] [algs #",\s+"]])]
      {::ingredients ings
       ::allergens algs})))

(defn read-input-chunks
  [{::keys [parse]
    :or {parse identity}
    :as ctx}]
  (let [r (read-input ctx)
        ln (clojure.string/split-lines r)
        chunks (->>
                (partition-by #(empty? %) ln)
                (remove (comp empty? first)))]
    chunks))

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
  [state z]
  (let [vls (keys state)
        [[x-mn x-mx]
         [y-mn y-mx]] (mapv
                       #(let [ps (mapv %1 vls)]
                          [(apply min ps) (apply max ps)])
                       [first second])
        s (for [x (range x-mn (inc x-mx))]
            (reduce
             (fn [s y]
               (let [v (get state [x y z])
                     c (cond
                         (= v :occupied) \#
                         (= v :empty) \.
                         (nil? v) \.)]
                 (str s c)))
             ""
             (range y-mn (inc y-mx))))]
    [[x-mn y-mx z] s]))

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

(defn parse-tickets
  [ctx]
  (let [read-ticket #(mapv read-string (clojure.string/split %1 #","))
        [r-header [_ r-ticket] [_ & r-nearby]] (read-input-chunks ctx)]
    {::header (let [hs (for [l r-header]
                         (let [[n desc] (clojure.string/split l #":\s+")
                               [p1 p2] (clojure.string/split desc #"\s+or\s+")
                               [r1 r2] (mapv
                                        #(mapv read-string (clojure.string/split %1 #"-"))
                                        [p1 p2])]
                           [(keyword n) [r1 r2]]))]
                (into {} hs))
     ::ticket (read-ticket r-ticket)
     ::nearby-tickets (mapv read-ticket r-nearby)}))

(defn eval-deps
  [deps i-i]
  (let [re-deps (reduce
                 (fn
                   [re [i coll]]
                   (reduce #(update %1 %2 (fnil conj #{}) i) re coll))
                 nil
                 deps)]
    (loop [i i-i
           re-deps re-deps
           deps deps
           rs {}]
      (if (= i nil)
        rs
        (let [d (-> (get deps i) first)
              re-deps* (update re-deps d disj i)
              deps* (dissoc deps i)
              [deps* nxt-i] (reduce
                             (fn [[deps* nxt] i]
                               (let [deps* (update deps* i disj d)
                                     new-ds (get deps* i)]
                                 (if (= 1 (count new-ds))
                                   [deps* i]
                                   [deps* nxt])))
                             [deps* nil]
                             (get re-deps d))]
          (recur
           nxt-i
           re-deps*
           deps*
           (assoc rs i d)))))))

(def deltas
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        dw (range -1 2)
        :when (or
               (not= dx 0)
               (not= dy 0)
               (not= dz 0)
               (not= dw 0))]
    [dx dy dz dw]))

(defn xyzw-search
  [coords]
  (map
   (fn [dxs]
     (map + coords dxs))
   deltas))

(defn update-cubes
  [s s* [x y z w]]
  (let [v (get s [x y z w])
        nbs (->>
             (xyzw-search [x y z w]))
        v-nbs (->>
                 nbs
                 (filter (comp #(= % :occupied) #(get s %))))
        nbs-cnt (count v-nbs)
        v* (cond
             (and (= v :occupied) (<= 2 nbs-cnt 3)) :occupied
             (and (nil? v) (= nbs-cnt 3)) :occupied
             :else nil)]
    (if (= v* :occupied)
      (assoc s* [x y z w] v*)
      s*)))

(defn cubes-eval
  [state limit]
  (let [xyzw-coords (fn [s]
                     (reduce
                      (fn [bag [[x y z w] _]]
                        (-> (apply conj (conj bag [x y z w]) (xyzw-search [x y z w]))))
                      #{}
                      s))]
    (loop [i 0
           s state]
      (if (< i limit)
        (let [s* (reduce
                  #(update-cubes s %1 %2)
                  {}
                  (xyzw-coords s))]
          (recur (inc i) s*))
        s))))

(defn eval-op
  [op x y]
  ((resolve op) x y))

(defn eval-p
  [coll j]
  (loop [v nil
         j j]
    (assert (or (nil? v) (int? v)) v)
    (let [x (nth coll j nil)]
      (cond
        (= x nil) [v j]
        (= x :end-p) [v (inc j)]
        (= x :open-p) (let [[v j] (eval-p coll (inc j))]
                        (recur
                         v
                         j))
        :else (cond
                (nil? v) (recur x (inc j))
                :else (let [j (inc j)
                            q (nth coll j)
                            j (inc j)
                            [q j] (if (= q :open-p)
                                    (eval-p coll j)
                                    [q j])]
                        (recur
                         (eval-op x v q)
                         j)))))))
(defn vec-displace
  [coll s e v]
  (vec
   (concat
    (subvec coll 0 s)
    [v]
    (subvec coll e))))

(defn eval-p2
  [coll]
  (let [coll (mapv
              (fn [part]
                (if (coll? part)
                  (eval-p2 part)
                  part))
              coll)
        safe-index (fn [coll p]
                     (let [j (.indexOf coll p)]
                       (if (= j -1)
                         nil
                         j)))]
    (loop [v nil
           coll coll]
      (let [j (or
               (safe-index coll '+)
               (safe-index coll '*))]
        (cond
          (nil? j) (or v (first coll))
          :else (let [[a b] [(dec j) (+ j 2)]
                      [x op y] (subvec coll a b)
                      v (eval-op op x y)]
                  (recur
                   v
                   (->
                    (vec-displace coll a b v)))))))))

(defn coll->tree
  [coll i]
  (loop [tree nil
         j i]
    (let [x (nth coll j nil)]
      (cond
        (= x nil) [(vec tree) j]
        (= x :open-p) (let [[tree* j] (coll->tree coll (inc j))]
                        (recur
                         (conj tree tree*)
                         j))
        (= x :end-p) [(vec tree) (inc j)]
        :else (recur
               (conj tree x)
               (inc j))))))

(defn edges-of
  [coll]
  (let [e1 (apply str coll)
        e2 (apply str (reverse coll))]
    (-> (sort [e1 e2]) first)))

(defn chunk->tile
  [[h & rows]]
  (let [id (re-find #"\d+" h)]
    {::id (read-string id)
     ::content rows
     ::edges {::top (edges-of (first rows))
              ::left (edges-of (map first rows))
              ::right (edges-of (map last rows))
              ::bottom (edges-of (last rows))}}))

(defn ingredient->allergen-parts
  [coll]
  (reduce
   (fn [m {::keys [ingredients
                   allergens]}]
     (reduce
      (fn [m i]
        (update m i (fnil conj []) allergens))
      m
      ingredients))
   {}
   coll))

(defn allergen->ingredient-parts
  [coll]
  (reduce
   (fn [m {::keys [ingredients
                   allergens]}]
     (reduce
      (fn [m i]
        (update m i (fnil conj []) ingredients))
      m
      allergens))
   {}
   coll))

(defn ing-deps
  [coll]
  (let [a-deps (allergen->ingredient-parts coll)
        i-deps (ingredient->allergen-parts coll)]
    [a-deps i-deps]))

(defn always-shows-with-ing
  [adp ing alg]
  (every?
   (fn [ings]
     (contains? ings ing))
   (get adp alg)))

(defn solve-1
  [coll]
  (let [[a-deps
         i-deps] (mapv
                  (fn [f] (f coll))
                  [allergen->ingredient-parts
                   ingredient->allergen-parts])
        safe-ings (->>
                   i-deps
                   (remove
                    (fn [[i algs]]
                      (let [algs (reduce clojure.set/union #{} algs)]
                        (some #(always-shows-with-ing a-deps i %) algs))))
                   (map first)
                   (into #{}))
        not-safe (-> i-deps keys set (clojure.set/difference safe-ings))
        r (->>
           coll
           (map (comp count #(clojure.set/intersection safe-ings %) ::ingredients)))]
    [(apply + r)
     r
     safe-ings
     not-safe]))


(defn solve-2
  [coll]
  (let [[a-deps
         i-deps] (mapv
                  (fn [f] (f coll))
                  [allergen->ingredient-parts
                   ingredient->allergen-parts])
        dang-ings (->>
                   i-deps
                   (reduce
                    (fn [r [i algs]]
                      (let [algs (reduce clojure.set/union #{} algs)
                            algs (filter
                                  #(let [v (always-shows-with-ing a-deps i %)]
                                     (if v
                                       %
                                       nil))
                                  algs)]
                        (if (empty? algs)
                          r
                          (conj r [i (set algs)]))))
                    nil))
        _ (prn dang-ings)
        ]
    nil))

(comment
  (def a-input
    (let [part :a
          r (read-input-lines
             {::part part
              ::parse parse-in-line
              })]
      r))
  a-input
  (ing-deps a-input)

  (nth (solve-1 a-input) 3)
  (count (nth (solve-1 a-input) 3))
  (solve-2 a-input)

  (->>
   [["bsqh" "nuts"]
    ["cxk" "fish"]
    ["lmzg" "dairy"]
    ["cpbzbx" "sesame"]
    ["kqprv" "wheat"]
    ["cfnt" "soy"]
    ["bdvmx" "peanuts"]
    ["drbm" "shellfish"]]
   (sort-by second)
   (map first)
   (clojure.string/join ",")
   )

  (def c-input
    (let [part :c
          r (read-input-lines
             {::part part
              ::parse parse-in-line
              })]
      r))
  c-input
  (ing-deps c-input)
  (solve-1 c-input)
  (solve-2 c-input)

  (def d-input
    (let [part :d
          r (read-input-lines
             {::part part
              ::raw raw
              ::parse parse-in-line})
          r (first r)]
      (coll->tree r 0)))
  d-input

)
