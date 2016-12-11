(ns day-10.core
  (:gen-class))

(def instrs
  ["value 5 goes to bot 2"
   "bot 2 gives low to bot 1 and high to bot 0"
   "value 3 goes to bot 1"
   "bot 1 gives low to output 1 and high to bot 0"
   "bot 0 gives low to output 2 and high to output 0"
   "value 2 goes to bot 2"])

(defn conj-s
  [s v]
  (if (nil? s)
    #{v}
    (conj s v)))

(defn value-fn
  [[val-s _ _ _ bot-s]]
  (let [v (Integer. val-s)
        bot (Integer. bot-s)]
    (fn
      [board]
      (update-in
        board
        [:bots bot]
        #(conj-s % v)))))

(defn high-low-fn
  [[bot-s _ _ _ low-type-s low-bot-s _ _ _ high-type-s high-bot-s]]
  (let [bot (Integer. bot-s)
        low-bot (Integer. low-bot-s)
        high-bot (Integer. high-bot-s)
        low-type (keyword (str low-type-s "s"))
        high-type (keyword (str high-type-s "s"))]
    (fn
      [board]
      (let [chips (get-in board [:bots bot])
            [low high] (sort chips)]
        (if (< (count chips) 2)
          board
          (->
            board
            (assoc-in [:bots bot] #{})
            (update-in [low-type low-bot] #(conj-s % low))
            (update-in [high-type high-bot] #(conj-s % high))))))))

(defn parse-cmd
  [cmd]
  (let [[fst & rst] (clojure.string/split cmd #"\s+")]
    (condp = fst
      "value" (value-fn rst)
      "bot" (high-low-fn rst))))

(defn apply-cmd
  [brd cmd]
  (cmd brd))

(defn run
  [board & cmds]
  (let [val-cmds
        (filter #(= (first (clojure.string/split % #"\s+")) "value") cmds)
        bot-cmds
        (filter #(= (first (clojure.string/split % #"\s+")) "bot") cmds)]
    (->>
      (concat val-cmds bot-cmds)
      (map parse-cmd)
      (reduce #(%2 %1) board))))

(defn bot-has
  [chips]
  (fn
    [board]
    (let [matches (filter #(= chips (second %)) (board :bots))]
      (if (= (count matches) 0)
        false
        true))))

(defn find-bot
  [chips board & cmds]
  (let [val-cmds
        (filter #(= (first (clojure.string/split % #"\s+")) "value") cmds)
        bot-cmds
        (filter #(= (first (clojure.string/split % #"\s+")) "bot") cmds)
        initial-board (reduce #(%2 %1) board (map parse-cmd val-cmds))]
      (->>
        bot-cmds
        (map parse-cmd)
        cycle
        (reductions #(%2 %1) initial-board)
        (map println))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    clojure.string/split-lines
    (apply find-bot #{61 17} {:bots {}})
    println))
