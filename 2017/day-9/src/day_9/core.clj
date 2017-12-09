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

(defn score''
  [s]
  (->>
    (iterate
      (fn
        [[ttl i d [tag & tags]]]
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
            ;(prn ttl i d tag c c')
            (condp = tag
              nil (condp = c'
                    :open-gr [(+ ttl d) i' (inc d) [:gr]]
                    :open-gb [ttl i' d [:gb]]
                    :not [ttl i' d [:not]]
                    [ttl i' d nil])
              :gr (condp = c'
                    :close-gr [ttl i' (dec d) tags]
                    :open-gr [(+ ttl d) i' (inc d) (push tags tag :gr)]
                    :open-gb [ttl i' d (push tags tag :gb)]
                    :not [ttl i' d (push tags tag)]
                    :any [ttl i' d (push tags tag)])
              :gb (condp = c'
                    :close-gb [ttl i' d tags]
                    :open-gr [ttl i' d (push tags tag)]
                    :close-gr [ttl i' d (push tags tag)]
                    :open-gb [ttl i' d (push tags tag)]
                    :not [ttl i' d (push tags tag :not)]
                    :any [ttl i' d (push tags tag)])
              :not [ttl i' d tags]))
          [ttl]))
      [0 0 1 nil])
    (take-while #(> (count %) 2))
    last
    first))

(defn score'''
  [s]
  (->>
    (iterate
      (fn
        [[ttl i d [tag & tags]]]
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
            ;(prn ttl i d tag c c')
            (condp = tag
              nil (condp = c'
                    :open-gr [ttl i' (inc d) [:gr]]
                    :open-gb [ttl i' d [:gb]]
                    :not [ttl i' d [:not]]
                    [ttl i' d nil])
              :gr (condp = c'
                    :close-gr [ttl i' (dec d) tags]
                    :open-gr [ttl i' (inc d) (push tags tag :gr)]
                    :open-gb [ttl i' d (push tags tag :gb)]
                    :not [ttl i' d (push tags tag)]
                    :any [ttl i' d (push tags tag)])
              :gb (condp = c'
                    :close-gb [ttl i' d tags]
                    :open-gb [(inc ttl) i' d (push tags tag)]
                    :open-gr [(inc ttl) i' d (push tags tag)]
                    :close-gr [(inc ttl) i' d (push tags tag)]
                    :not [ttl i' d (push tags tag :not)]
                    :any [(inc ttl) i' d (push tags tag)])
              :not [ttl i' d tags]))
          [ttl]))
      [0 0 1 nil])
    (take-while #(> (count %) 2))
    last
    first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
