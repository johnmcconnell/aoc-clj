(ns day-7.core
  (:gen-class))

(defn aba?
  [s]
  (and
    (= (nth s 0) (nth s 2))
    (every? #(not= \[ %) s)
    (every? #(not= \] %) s)
    (not= (nth s 0) (nth s 1))))

(defn bab?
  [s1 s2]
  (and
    (aba? s1)
    (= (set s1) (set s2))))

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

(defn fill-ssl-props
  [{in-parens :in-parens invalid :invalid :as state} s]
  (cond
    (= (first s) \[) (assoc state :in-parens true)
    (= (first s) \]) (assoc state :in-parens false)
    (and in-parens (aba? s))
      (assoc state :babs (conj (state :babs) s))
    (and (not in-parens) (aba? s))
      (assoc state :abas (conj (state :abas) s))
    :else state))

(defn valid-props?
  [{valid :valid invalid :invalid :as props}]
  (and
    (= true valid)
    (not invalid)))

(defn bab->aba
  [[b a _]]
  [a b a])

(defn valid-ssl-props?
  [{abas :abas babs :babs}]
  (let [t-babs (set (map bab->aba babs))
        result (->
                 (filter #(t-babs %) abas)
                 count
                 (> 0))]
    result))

(defn supports-tls?
  [s]
  (->>
    s
    (partition 4 1)
    (reduce fill-props {:invalid false})
    valid-props?))

(defn supports-ssl?
  [s]
  (->>
    s
    (partition 3 1)
    (reduce fill-ssl-props {:abas [] :babs [] :invalid false})
    valid-ssl-props?))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (slurp *in*)
    (clojure.string/split-lines)
    (filter supports-ssl?)
    count
    println))
