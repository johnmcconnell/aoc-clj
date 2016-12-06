(ns day-6.core
  (:gen-class))

(defn get-idx
  [idx]
  #(nth % idx))

(defn by-column
  [lines]
  (let [msg-cnt (count (first lines))]
    (->
      (map
        #(->>
           (map (get-idx %) lines)
           (apply str))
        (range 0 msg-cnt)))))

(defn most-feq-char
  [string]
  (->> (frequencies string)
       (sort-by val >)
       (take 1)
       first))

(defn least-feq-char
  [string]
  (->> (frequencies string)
       (sort-by val <)
       (take 1)
       first))

(defn corrected-message
  [input]
  (->> input
       clojure.string/split-lines
       by-column
       (map most-feq-char)
       (map first)
       (apply str)))

(defn corrected-message-2
  [input]
  (->> input
       clojure.string/split-lines
       by-column
       (map least-feq-char)
       (map first)
       (apply str)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    corrected-message-2
    println))
