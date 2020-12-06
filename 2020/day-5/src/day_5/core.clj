(ns day-5.core
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
    (let [[row-s col-s] (split-at 7 l)]
      (mapv
       (fn [s]
         (->
          (reduce #(str %1 (get {\F 0 \B 1 \R 1 \L 0} %2)) "" s)
          (Integer/parseInt 2)))
       [row-s col-s]))))

(defn seat-id
  [[row col]]
  (+ (* 8 row) col))

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

(defn find-missing
  [seats]
  (loop [pv (dec (first seats))
         i 0]
    (if (= i (count seats))
      -1
      (let [v (nth seats i)
            dx (- v pv)]
        (if (> dx 1)
          v
          (recur v (inc i)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (def a-input
    (read-input-lines
     {::part :a
      ::parse parse-in-line}))

  (parse-in-line "BFFFBBFRRR")
  (def seats (mapv seat-id a-input))
  (apply max seats)
  (find-missing (-> seats sort vec))
  (sort seats)

  (def c-input
    (read-input-lines
     {::part :c
      ::parse identity}))

  )
