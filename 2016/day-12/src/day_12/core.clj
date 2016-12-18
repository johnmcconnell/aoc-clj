(ns day-12.core
  (:gen-class))

(defn cpy
  [rst]
  (let [id (-> rst (nth 1) keyword)
        val-s (-> rst (nth 0))
        num-match (re-matches #"-*\d+" val-s)]
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
        num-match (re-matches #"-*\d+" val-s)]
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

(defn exec-it
  [{:keys [stack_ptr stack] :as state}]
  (let [f (->
            stack_ptr
            stack
            parse)]
    (->
      state
      f
      (update :stack_ptr inc))))

(defn below-stack?
  [{:keys [stack_ptr stack]}]
  (< stack_ptr (count stack)))

(defn exec
  [instrs]
  (->>
    (iterate exec-it {:stack_ptr 0
                      :stack instrs
                      :a 0
                      :b 0
                      :c 1
                      :d 0})
    (drop-while below-stack?)
    first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    clojure.string/split-lines
    exec
    println))
