(ns day-9.core
  (:gen-class))


(defn conj-c
  ([c] (conj-c c 1))
  ([c i]
   #(vec (apply conj % (repeat i c)))))

(defn decomp-char
  [{:keys [res act rep-cnt char-cnt decomp-str] :as state} c]
  (condp = act
    :bgn-parse (condp = c
                 \x (assoc state :act :nxt-parse)
                 (update-in state [:char-cnt] (conj-c c)))
    :nxt-parse (condp = c
                 \) (assoc state :act :end-parse)
                 (update-in state [:rep-cnt] (conj-c c)))
    :end-parse (let [rep-cnt-i (Integer. (apply str rep-cnt))
                     char-cnt-i (Integer. (apply str char-cnt))
                     instr {:rep-cnt rep-cnt-i
                            :char-cnt char-cnt-i}]
                 (condp = char-cnt-i
                   0 (->
                       state
                       (update-in [:res] (conj-c c))
                       (assoc :act nil))
                   1 (->
                       state
                       (update-in [:res] (conj-c c rep-cnt-i))
                       (assoc :act nil))
                   (->
                     state
                     (update-in [:decomp-str] (conj-c c))
                     (assoc :act :gather-decomp)
                     (merge instr))))
    :gather-decomp (if (= char-cnt (inc (count decomp-str)))
                     (->
                       state
                       (update-in
                         [:res]
                         #(vec
                            (concat
                              %
                              (flatten
                                (repeat
                                  rep-cnt
                                  (conj decomp-str c))))))
                       (assoc :act nil)
                       (assoc :decomp-str nil)
                       (assoc :rep-cnt nil)
                       (assoc :char-cnt nil))
                     (update-in state [:decomp-str] (conj-c c)))
    (condp = c
      \( (assoc state :act :bgn-parse)
      (update-in state [:res] (conj-c c)))))

(defn decompress
  [s]
  (->>
    s
    (reduce decomp-char {:res []})
    :res
    (apply str)))

(defn subsec
  ([s start] (drop start s))
  ([s start end]
   (->>
     s
     (drop start)
     (take (- end start)))))

(defn parse-instr
  [s]
  (when s
    (let [[char-cnt rep-cnt] (clojure.string/split s #"x")
          char-cnt-i (Integer. (subs char-cnt 1))
          rep-cnt-i   (Integer.  (subs rep-cnt 0 (dec (count rep-cnt))))]
      {:char-cnt char-cnt-i :rep-cnt rep-cnt-i})))

(defn next-s
  [s]
  (let [[match rst] (clojure.string/split s #"\(\d+x\d+\)" 2)]
    {:cnt (count match) :next-s rst}))

(defn find-instr
  [s]
  (let [instr-s (re-find #"\(\d+x\d+\)" s)
        instr (parse-instr instr-s)]
    (if (nil? instr)
      {:cnt (count s) :done true}
      (merge instr (next-s s)))))

(defn decompress-2
  [s]
  (let [{:keys [cnt done char-cnt rep-cnt next-s]} (find-instr s)]
    ;(println cnt done char-cnt rep-cnt next-s)
    (if done
      cnt
      (+
       cnt
       (* rep-cnt (decompress-2 (subs next-s 0 char-cnt)))
       (decompress-2 (subs next-s char-cnt))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    (remove #(re-matches #"\s+" (str %)))
    (apply str)
    decompress-2
    println))
