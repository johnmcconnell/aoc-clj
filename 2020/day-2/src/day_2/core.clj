(ns day-2.core
  (:gen-class))

(defn read-input
  []
  (->>
   "day-2.a.input"
   slurp
   clojure.string/split-lines
   (map
    (fn
      [s]
      (let [[l r] (clojure.string/split s #": ")
            [mn mx ch] (clojure.string/split l #"-| ")]
        [(read-string mn) (read-string mx) (first ch) r])))))

(defn is-valid?
  [mn mx ch s]
  (let [fqs (frequencies s)
        cnt (get fqs ch 0)]
    (and
     (<= mn cnt)
     (<= cnt mx))))


(def input (read-input))
(def results (filter #(apply is-valid? %) input))
(count results)

(defn is-valid-2?
  [mn mx ch s]
  (let [chs
        (->>
         (map #(get s (dec %)) [mn mx])
         (filter #(= ch %)))]
    (= (count chs) 1)))

(def results-2 (filter #(apply is-valid-2? %) input))
(count results-2)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
