(ns day-4.core
  (:require
    [clojure.data])
  (:gen-class))

(def my-i
  (->>
    "input.txt"
    slurp))

(def my-i' (clojure.string/split-lines my-i))

(defn ->cmd
  [[x & xs]]
  (cond
    (= (count xs) 1) [x nil]
    :else ["shift" (first xs)]))

(defn ->log
  [s]
  (let [[d t & rst] (clojure.string/split s #"\s+")
        [y mn d] (as-> (subs d 1) $
                   (clojure.string/split $ #"-")
                   (map #(Integer/parseInt %) $))
        [h m] (as-> (subs t 0 (-> t count dec)) $
                (clojure.string/split $ #":")
                (map #(Integer/parseInt %) $))]
    [[y mn d h m] (->cmd rst)]))

(def logs
  (as-> my-i' $
    (map ->log $)
    (sort-by first $)))

(defn minute-
  [t1 t2]
  (let [deltas (map - t1 t2)
        weights [(* 365 30 24 60) (* 30 24 60) (* 24 60) 60 1]
        cnts (map * weights deltas)
        v (apply + cnts)]
    v))

(defn assoc-sleep-guard
  [[ttl minutes] t pt]
  (let [min-cnt (minute- t pt)
        [_ _ _ _ sm] pt
        a-cnt (quot min-cnt 60)
        r (rem min-cnt 60)
        offset (+ r sm)
        lows (set (range sm (min offset 60)))
        highs (set (if (> sm 60) (range 0 (mod offset 60)) nil))]
    [((fnil + 0) ttl min-cnt)
     (reduce
       (fn [m i]
         (update
           m
           i
           (fnil + 0)
           (cond-> a-cnt
             (contains? lows i) inc
             (contains? highs i) inc)))
       minutes
       (range 0 60))]))

(defn part-1
  []
  (as-> logs $
    (reduce
      (fn
        [[sleep-cnt pg pt] [t [cmd v]]]
        (cond
          (= cmd "shift") [sleep-cnt v t]
          (= cmd "wakes") [(update sleep-cnt pg assoc-sleep-guard t pt) pg t]
          (= cmd "falls") [sleep-cnt pg t]))
      [{} nil nil]
      $)
    (sort-by (comp first second) (first $))
    (last $)))

(defn part-2
  []
  (as-> logs $
    (reduce
      (fn
        [[sleep-cnt pg pt] [t [cmd v]]]
        (cond
          (= cmd "shift") [sleep-cnt v t]
          (= cmd "wakes") [(update sleep-cnt pg assoc-sleep-guard t pt) pg t]
          (= cmd "falls") [sleep-cnt pg t]))
      [{} nil nil]
      $)
    (map (fn [[x y]] [x (second y)]) (first $))
    (map (fn [[x mns]] [x (->> mns (sort-by second) last)]) $)
    (sort-by (comp second second) $)
    (last $)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
