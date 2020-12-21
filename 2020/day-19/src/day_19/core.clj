(ns day-19.core
  (:require [blancas.kern.core :as kern])
  (:gen-class))

(defn valid-num?
  [mn mx]
  (fn [v]
    (let [v* (read-string v)]
      (and
       (<= mn v*)
       (<= v* mx)))))

(defn valid-reg?
  [re]
  (fn [s]
    (re-matches re s)))

(defn read-input
  [{::keys [part
            raw]
    :or {part :a}}]
  (if (nil? raw)
    (let [f-name (str (name part) ".input")]
      (-> f-name slurp))
    raw))

(defn read-input-lines
  [{::keys [parse]
    :or {parse identity}
    :as ctx}]
  (->>
   (read-input ctx)
   clojure.string/split-lines
   (map parse)))

(defn parse-in-line
  [l]
  (if (empty? l)
    nil
    (let [parts (clojure.string/split l #"\s+")
          symbols (mapcat
                   (fn [word]
                     (let [l (count word)
                           [h-cnt t-cnt] (mapv
                                          #(inc
                                            ((fnil - 0 1)
                                             (clojure.string/last-index-of word %)
                                             (clojure.string/index-of word %)))
                                          [\( \)])
                           word (subs word h-cnt (- l t-cnt))]
                       (concat
                        (repeat h-cnt :open-p)
                        [(read-string word)]
                        (repeat t-cnt :end-p))))
                     parts)]
      symbols)))

(defn read-input-chunks
  [{::keys [parse]
    :or {parse identity}
    :as ctx}]
  (let [r (read-input ctx)
        ln (clojure.string/split-lines r)
        chunks (->>
                (partition-by #(empty? %) ln)
                (remove (comp empty? first)))]
    chunks))


(defn compile-rule
  [cache rules id]
  (cond
    (contains? cache id) cache
    :else (let [{::keys [or-clauses]} (get rules id)
                _ (assert (> (count or-clauses) 0) or-clauses)
                [cache or-clauses] (reduce
                                    (fn [[cache clauses] clause]
                                      (let [[cache literals] (reduce
                                                              (fn [[cache lits] lit]
                                                                (if (string? lit)
                                                                  [cache (conj lits (kern/token* lit))]
                                                                  (let [cache (compile-rule cache rules lit)]
                                                                    [cache (conj lits (get cache lit))])))
                                                              [cache []]
                                                              clause)
                                            clause (kern/<:> (apply kern/<+> literals))]
                                        [cache (conj clauses clause)]))
                                    [cache []]
                                    or-clauses)
                or-clauses (if (= 1 (count or-clauses))
                             (first or-clauses)
                             (apply kern/<|> or-clauses))]
            (assoc cache id or-clauses))))

(defonce my-cache (atom {}))

(defn ref-p
  [lit]
  (fn [x]
    (let [p (get @my-cache lit)]
      ((kern/<:> p) x))))

(defn compile-rule-2
  [rules id]
  (let [{::keys [or-clauses]} (get rules id)
        _ (assert (> (count or-clauses) 0) or-clauses)
        or-clauses (reduce
                    (fn [clauses clause]
                      (let [literals (reduce
                                      (fn [lits lit]
                                        (if (string? lit)
                                          (conj lits (kern/token* lit))
                                          (conj lits (ref-p lit))))
                                      []
                                      clause)
                            clause (kern/<:> (apply kern/<+> literals))]
                        (conj clauses clause)))
                    []
                    or-clauses)
        or-clauses (if (= 1 (count or-clauses))
                     (first or-clauses)
                     (apply kern/<|> or-clauses))]
    (swap! my-cache #(assoc % id or-clauses))))

(defn build-parser
  [{::keys [rules]} id]
  (->
   (compile-rule rules 0)
   (get id)))

(defn load-parsers!
  [{::keys [rules]}]
  (doseq [[k _] rules]
   (compile-rule-2 rules k)))

(defn test-p
  [parser txt]
  (let [p (kern/<+> parser kern/eof)]
    (kern/parse p txt)))

(defn filter-with-parser
  [{::keys [body]} parser]
  (let [p (kern/<+> parser kern/eof)]
    (filter
     (fn [txt]
       (let [r (kern/parse p txt)
             ok (:ok r)]
         ok))
     body)))

(comment
  )
