(ns day-20.core
  (:gen-class))

(defn split-slot
  [[b-min b-max]]
  (fn
    [[w-min w-max]]
    (if (< w-min b-min)
      (if (> w-max b-max)
        [[w-min (dec b-min)]
         [(inc b-max) w-max]]
        [[w-min (min (dec b-min) w-max)]])
      (if (> w-max b-max)
        [[(max (inc b-max) w-min) w-max]]
        []))))

(defn conj-available
  [time-slots busy-slot]
  (let [r (mapcat (split-slot busy-slot) time-slots)]
    r))

(defn all-available
  [time-slots busy-slots]
  (reduce conj-available time-slots busy-slots))

(defn low-ip
  [m h bl]
  (->>
    (all-available [[m h]] bl)
    (map #(first %))
    sort
    first))

(defn count-ip
  [m h bl]
  (->>
    (all-available [[m h]] bl)
    (mapcat #(range (first %) (inc (second %))))
    count))

(defn parse-s-s
  [s]
  (let [[low high] (->>
                     (clojure.string/split s #"-")
                     (map #(BigInteger. %)))]
    [low high]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    clojure.string/split-lines
    (map parse-s-s)
    (count-ip 0 4294967295)
    println))
