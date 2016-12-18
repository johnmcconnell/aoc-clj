(ns day-12.core
  (:gen-class))

(defn cpy
  [rst]
  (let [id (-> rst (nth 1) keyword)
        val-s (-> rst (nth 0))
        num-match (re-matches #"\d+" val-s)]
    (fn
      [state]
      (->>
        (if num-match
          (Integer. val-s)
          (-> val-s keyword state))
        (assoc state id)))))

(defn up-f
  [rst f]
  (fn
    [state]
    (->
      state
      (update
        (-> rst (nth 0) keyword) f))))

(defn jnz
  [rst]
  (let [offset (-> rst (nth 1) Integer. (- 1))
        val-s (-> rst (nth 0))
        num-match (re-matches #"\d+" val-s)]
    (fn
      [state]
      (let [v (if num-match
                (Integer. val-s)
                (-> val-s keyword state))]
        (if (= v 0)
          state
          (update state :stack_ptr #(+ % offset)))))))

(defn parse
  [s]
  (let [parts (clojure.string/split s #"\s+")
        fst (first parts)
        rst (rest parts)]
    (condp = fst
      "cpy" (cpy rst)
      "inc" (up-f rst inc)
      "dec" (up-f rst dec)
      "jnz" (jnz rst))))

(defn exec
  ([instrs] (exec instrs {:stack_ptr 0 :stack instrs}))
  ([instrs {:keys [stack_ptr stack] :as state}]
   (if (< stack_ptr (count stack))
     (let [f (->
               stack_ptr
               stack
               parse)
           nxt-state (->
                       state
                       f
                       (update :stack_ptr inc))]
       (exec instrs nxt-state))
     state)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
