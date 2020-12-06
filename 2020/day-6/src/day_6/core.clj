(ns day-6.core
  (:gen-class))

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
    (into #{} l)))

(defn people->groups
  [coll]
  (->>
   (partition-by nil? coll)
   (mapv #(reduce clojure.set/union #{} %1))))

(defn people->groups-2
  [coll]
  (->>
   (partition-by nil? coll)
   (mapv #(reduce clojure.set/intersection %1))))

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
  (->>
   (people->groups-2 a-input)
   (mapv count)
   (apply +))
  c-input

  (parse-in-line "BFFFBBFRRR")
  (def seats (mapv seat-id a-input))
  (apply max seats)
  (find-missing (-> seats sort vec))
  (sort seats)

  (re-matches #"(a|b){5}" "abab")

  (def c-input
    (read-input-lines
     {::part :c
      ::parse parse-in-line}))

  )
