(ns day-7.core
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
    (let [[head bags] (clojure.string/split l #" contain ")
          bag-deps (clojure.string/split bags #", ")
          ->bag (fn [part]
                  (let [[_ cnt kind color] (re-matches #"(?<count>\d+)?\s*(?<kind>\w+) (?<color>\w+) bag(s)?.*" part)]
                    (cond-> {:id {:kind (keyword kind)
                                  :color (keyword color)}}
                      (some? cnt) (assoc :count (read-string cnt)))))]
      (if (= bag-deps ["no other bags."])
        [(->bag head) nil]
        [(->bag head) (mapv ->bag bag-deps)]))))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (def a-input
    (read-input-lines
     {::part :a
      ::parse parse-in-line}))

  (partition-by nil? c-input)

  (re-matches #".+(?<count>\d+\s?)?(?<kind>\w+) (?<color>\w+) bag(s)?\s+?" "123 faded blue bags ")

  (def c-needle {:kind :shiny :color :gold})
  (def c-input
    (read-input-lines
     {::part :c
      ::parse parse-in-line}))
  c-input

  (find-all-parents (reverse-deps c-input) c-needle)


  (find-all-parents (reverse-deps a-input) c-needle)

  (def a *1)

  (def d-input
    (read-input-lines
     {::part :d
      ::parse parse-in-line}))
  (get (reg-deps d-input) c-needle)
  (def x (resolve-deps (reg-deps d-input) {} {:count 0 :id c-needle}))
  (keys x)

  (->>
   (get (resolve-deps (reg-deps a-input) {} {:count 1 :id c-needle}) c-needle)
   (mapv :count)
   (apply +)
   )


  (count a)
  )
