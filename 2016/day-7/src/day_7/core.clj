(ns day-7.core
  (:gen-class))

(defn aba?
  [s]
  (and
    (= (nth s 0) (nth s 2))
    (not= (nth s 0) (nth s 1))))

(defn abba?
  [s]
  (and
    (= (nth s 0) (nth s 3))
    (= (nth s 1) (nth s 2))
    (not= (nth s 0) (nth s 1))))

(defn fill-props
  [{in-parens :in-parens invalid :invalid :as state} s]
  (cond
    (= (first s) \[) (assoc state :in-parens true)
    (= (first s) \]) (assoc state :in-parens false)
    (and
      (not invalid)
      (not in-parens)
      (abba? s)) (assoc state :valid true)
    (and in-parens (abba? s)) (assoc state :invalid true)
    :else state))

(defn valid-props?
  [{valid :valid invalid :invalid :as props}]
  (and
    (= true valid)
    (not invalid)))

(defn supports-tls?
  [s]
  (->>
    s
    (partition 4 1)
    (reduce fill-props {:invalid false})
    valid-props?))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (slurp *in*)
    (clojure.string/split-lines)
    (filter supports-tls?)
    count
    println))
