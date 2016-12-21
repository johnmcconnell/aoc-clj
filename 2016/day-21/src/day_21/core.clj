(ns day-21.core
  (:gen-class))

(defn ->I
  [s]
  (Integer. (str s)))

(defn swap-position
  [[is1 _ _ is2]]
  (fn
    [s]
    (let [v (vec s)
          [i1 i2] (map ->I [is1 is2])
          [c1 c2] (map #(get v %) [i1 i2])]
      (->
        v
        (assoc i1 c2)
        (assoc i2 c1)))))

(defn swap-letter
  [[l1 _ _ l2]]
  (fn
    [s]
    (map
      (fn
        [c]
        (condp = (str c)
          l1 l2
          l2 l1
          c)) s)))

(defn reverse-pos
  [[s1 _ s2]]
  (let [[i1 i2] (map ->I [s1 s2])]
    (fn
      [s]
      (let [s (apply str s)
            b (subs s 0 i1)
            m (apply str (reverse (subs s i1 (inc i2))))
            e (subs s (inc i2))]
        (str b m e)))))

(defn rot<-
  [[s _]]
  (let [i (->I s)]
    (fn
      [s]
      (->>
        s
        cycle
        (drop i)
        (take (count s))))))

(defn rot->
  [[s _]]
  (let [i (->I s)]
    (fn
      [s]
      (->>
        s
        reverse
        cycle
        (drop i)
        (take (count s))
        reverse))))

(defn move-pos
  [[s1 _ _ s2]]
  (let [[i1 i2] (map ->I [s1 s2])]
    (fn
      [s]
      (let [s (apply str s)
            c (str (get s i1))
            s1 (subs s 0 i1)
            s2 (subs s (inc i1))
            s (str s1 s2)
            s1 (subs s 0 i2)
            s2 (subs s i2)]
        (str s1 c s2)))))

(defn rot-based
  [[_ _ _ _ l]]
  (fn
    [s]
    (let [s (apply str s)
          idx (clojure.string/index-of s l)]
      (if (> idx 3)
        ((rot-> [(+ idx 2) nil]) s)
        ((rot-> [(+ idx 1) nil]) s)))))

(defn parse
  [instr]
  (let [[fst snd & rst] (clojure.string/split instr #"\s+")]
    (condp = [fst snd]
      ["swap" "position"] (swap-position rst)
      ["swap" "letter"] (swap-letter rst)
      ["reverse" "positions"] (reverse-pos rst)
      ["rotate" "left"] (rot<- rst)
      ["rotate" "right"] (rot-> rst)
      ["move" "position"] (move-pos rst)
      ["rotate" "based"] (rot-based rst))))

(defn scramble
  [s instrs]
  (->>
    (map parse instrs)
    (reduce (fn [s f] (f s)) (map identity s))
    (apply str)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    clojure.string/split-lines
    (scramble "abcdefgh")
    println))
