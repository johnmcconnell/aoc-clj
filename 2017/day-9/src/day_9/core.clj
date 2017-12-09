(ns day-9.core
  (:gen-class))

(defn g-s [] (slurp "input.txt"))
(defn g-ls [] (-> (g-s) (clojure.string/split-lines)))
(defn g-wrds [] (-> (g-s) (clojure.string/split #"\s+")))
(defn g-ls-wrds
  []
  (->>
    (g-ls)
    (map #(clojure.string/split #"\s+"))))

(defn score'
  ([s i] (score' s i 1 nil))
  ([s i d [tag & tags]]
   (if (< i (count s))
     (let [c (get s i)
           i' (inc i)
           c' (condp = c
                \! :not
                \< :open-gb
                \> :close-gb
                \{ :open-gr
                \} :close-gr
                :any)
           push (fn [t2s & t1s]
                  (concat (reverse t1s) t2s))]
       (prn i (count s))
       (condp = tag
         nil (condp = c'
               :open-gr (+ d (score' s i' (inc d) [:gr]))
               :open-gb (score' s i' d [:gb])
               :not (score' s i' d [:not])
               (score' s i' d nil))
         :gr (condp = c'
               :close-gr (score' s i' (dec d) tags)
               :open-gr (+ d (score' s i' (inc d) (push tags tag :gr)))
               :open-gb (score' s i' d (push tags tag :gb))
               :not (score' s i' d (push tags tag :not))
               :any (score' s i' d (push tags tag)))
         :gb (condp = c'
               :close-gb (score' s i' d tags)
               :open-gr (score' s i' d (push tags tag))
               :close-gr (score' s i' d (push tags tag))
               :open-gb (score' s i' d (push tags tag :gb))
               :not (score' s i' d (push tags tag :not))
               :any (score' s i' d (push tags tag)))
         :not (score' s i' d tags)))
     0)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
