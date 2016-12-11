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

(defn conj-c-2
  ([c] (conj-c-2 c 1))
  ([c i]
   #(apply conj % (repeat i c))))

(defn expand
  [{rep-cnt :rep-cnt char-cnt :char-cnt}]
  #(concat
     (flatten
       (repeat (dec rep-cnt) (take char-cnt %))) %))

(defn decomp-2-char
  [{:keys [res cnt act s rep-cnt char-cnt] :as state}]
  (let [[c & rst] s]
    (condp = act
      :bgn-parse (condp = c
                   \x (->
                        state
                        (assoc :act :nxt-parse)
                        (update-in [:s] rest))
                   (->
                     state
                     (update-in [:char-cnt] (conj-c-2 c))
                     (update-in [:s] rest)))
      :nxt-parse (condp = c
                   \) (->
                        state
                        (assoc :act :end-parse)
                        (update-in [:s] rest))
                   (->
                     state
                     (update-in [:rep-cnt] (conj-c-2 c))
                     (update-in [:s] rest)))
      :end-parse (let [rep-cnt-i (->>
                                   rep-cnt
                                   reverse
                                   (apply str)
                                   Integer.)
                       char-cnt-i (->>
                                    char-cnt
                                    reverse
                                    (apply str)
                                    Integer.)
                       instr {:rep-cnt rep-cnt-i
                              :char-cnt char-cnt-i}]
                   (->
                     state
                     (update-in [:s] (expand instr))
                     (assoc :act nil)
                     (assoc :rep-cnt nil)
                     (assoc :char-cnt nil)))
      (condp = c
        \( (->
             state
             (assoc :act :bgn-parse)
             (update-in [:s] rest))
        (->
          state
          (update-in [:cnt] inc)
          (update-in [:s] rest))))))

(defn decompress-2
  [s]
  (->>
    (iterate decomp-2-char {:s s :cnt 1})
    (take-while #(not (empty? (% :s))))
    last
    :cnt))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    *in*
    slurp
    (remove #(re-matches #"\s+" (str %)))
    (apply str)
    decompress
    count
    println))
