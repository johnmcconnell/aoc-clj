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

(defn merge-col
  [mx-2 r-idx]
  (if-let [row-2 (nth mx-2 r-idx nil)]
    (fn
      [c-idx v1]
      (if-let [v2 (nth row-2 c-idx nil)]
        v2
        v1))
    (fn
      [c-idx v1]
      v1)))

(defn merge-row
  [mx-2]
  (fn
    [r-idx row]
    (map-indexed
      (merge-col mx-2 r-idx) row)))

(defn merge-mx
  [mx mx-2]
  (map-indexed (merge-row mx-2) mx))


(defn rect-a-b
  [mx s]
  (let [[r-cnt c-cnt] (->>
                        (clojure.string/split s #"x")
                        (map #(Integer. %)))
        fill (fill-mx 1 r-cnt c-cnt)]
    (merge-mx mx fill)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
