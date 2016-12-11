(ns day-10.core
  (:gen-class))

(def init-state
  {:bots {1 [3]
          2 [2 5]}})

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
        (->
          board
          (assoc-in [:bots bot] #{})
          (update-in [low-type low-bot] #(conj-s % low))
          (update-in [high-type high-bot] #(conj-s % high)))))))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
