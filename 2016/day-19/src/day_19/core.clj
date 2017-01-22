(ns day-19.core
  (:gen-class))

(defn next-pos
  [idx elves]
  (->>
    [idx 0]
    (iterate
      (fn
        [[i cnt]]
        (let [n-i (mod (inc i) (count elves))]
          (if (= (nth elves n-i) 1)
            [n-i (inc cnt)]
            [n-i cnt]))))
    (drop-while
      #(< (second %) 2))
    first
    first))


(defn iter
  [[pos step elves cnt]]
  (println cnt)
  (let [ps (range pos (count elves) step)
        last-idx (last ps)
        [n-elves n-cnt] (->>
                          ps
                          (reduce
                            (fn
                              [[v c] p]
                              [(assoc v p 0) (dec c)])
                            [elves cnt]))
        n-pos (next-pos last-idx n-elves)]
    [n-pos (* step 2) n-elves n-cnt]))


(defn multiple-elves?
  [[_ _ _ cnt]]
  (> cnt 1))

(defn remaining-pos
  [[pos step elves cnt]]
  (inc (.indexOf elves 1)))

(defn elf-with-presents
  [elves]
  (->>
    (iterate iter [1 2 elves (count elves)])
    (drop-while multiple-elves?)
    first
    remaining-pos))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (repeat 3005290 1)
    vec
    elf-with-presents
    println))
