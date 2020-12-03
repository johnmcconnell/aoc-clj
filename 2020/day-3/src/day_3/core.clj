(ns day-3.core
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

(defn count-trees
  [trees]
  (let [d-size (count trees)
        l-size (-> trees first count)]
    (loop [i 1
           j 3
           t-cnt 0]
      (if (>= i d-size)
        t-cnt
        (recur
         (+ i 1)
         (mod (+ j 3) l-size)
         (cond-> t-cnt
           (= (get-in trees [i j]) :tree) inc))))))

(defn count-trees-2
  [trees [dy dx]]
  (let [d-size (count trees)
        l-size (-> trees first count)]
    (loop [i dy
           j dx
           t-cnt 0]
      (if (>= i d-size)
        t-cnt
        (recur
         (+ i dy)
         (mod (+ j dx) l-size)
         (cond-> t-cnt
           (= (get-in trees [i j]) :tree) inc))))))

(defn count-all
  [trees params]
  (->>
   (mapv #(count-trees-2 trees %) params)
   (apply *)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (def a-input
    (read-input-lines
     {::part :a
      ::parse (fn [l] (mapv #(get {\# :tree \. :open} %) l))}))

  (count-all
   (vec a-input)
   [[1 1] [1 3] [1 5] [1 7] [2 1]])

  (def c-input
    (read-input-lines
     {::part :c
      ::parse (fn [l] (mapv #(get {\# :tree \. :open} %) l))}))

  )
