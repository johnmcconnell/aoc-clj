(ns day-4.core
  (:gen-class))

(defn read-input
  [{::keys [part]
    :or {part :a}}]
  (let [f-name (str (name part) ".input")]
    (-> f-name slurp)))

(defn read-input-lines
  [{::keys [parse]
    :or {parse identity}
    :as ctx}]
  (->>
   (read-input ctx)
   clojure.string/split-lines
   (map parse)))

(defn parse-p-line
  [l]
  (if (= l "")
    nil
    (let [wds (clojure.string/split l #"\s+")
          keys (reduce
                (fn
                  [ss w]
                  (let [[k v] (clojure.string/split w #":")]
                    (assoc ss k v)))
                {}
                wds)]
      keys)))

(defn is-valid-p1?
  [p]
  (let [wl #{"byr"
             "iyr"
             "eyr"
             "hgt"
             "hcl"
             "ecl"
             "pid"}
        ks (-> p keys set)]
    (= (clojure.set/intersection ks wl) wl)))

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

(defn is-valid-p2?
  [p]
  (let [wl {"byr" (valid-num? 1920 2002)
            "iyr" (valid-num? 2010 2020)
            "eyr" (valid-num? 2020 2030)
            "hgt" (fn [s]
                    (let [[_ hgt unit] (re-find #"(.*)(cm|in)" s)]
                      (cond
                        (nil? unit) false
                        (= "in" unit) ((valid-num? 59 76) hgt)
                        (= "cm" unit) ((valid-num? 150 193) hgt)
                        )))
            "hcl" (valid-reg? #"#[0-9a-f]{6}")
            "ecl" (fn [s]
                    (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))
            "pid" (valid-reg? #"[0-9]{9}")}]
    (every?
     (fn [[k vdr]]
       (let [v (get p k)]
         (if (nil? v)
           false
           (vdr v))))
     wl)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (read-input-lines {})

  (def a-input
    (let [[p pps] (->>
               (read-input-lines
                {::part :a
                 ::parse parse-p-line})
               (reduce
                (fn [[x xs] p-part]
                  (if (nil? p-part)
                    [nil (conj xs x)]
                    [(merge x p-part) xs]))
                [nil []]))]
      (conj pps p)))

  (def m (re-matcher #"(.*)(cm|in)" "234"))
  (re-find #"(.*)(cm|in)" "234cm")

  (->>
   a-input
   (filter is-valid-p1?)
   count)

  (->>
   a-input
   (filter is-valid-p2?)
   count)

  (def c-input
    (read-input-lines
     {::part :c
      ::parse identity}))

  )
