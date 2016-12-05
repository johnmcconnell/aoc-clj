(ns day-4.core
  (:gen-class))

(defn parse-token
  [input]
  (let [checksum-st-idx (.lastIndexOf input "[")
        sector-id-st-idx (.lastIndexOf input "-")
        ciphertext (subs input 0 sector-id-st-idx)
        sector-id-str (subs
                        input (inc sector-id-st-idx) checksum-st-idx)
        checksum (subs
                   input (inc checksum-st-idx) (dec (count input)))
        sector-id (Integer. sector-id-str)]
    {:ciphertext ciphertext
     :sector-id sector-id
     :checksum checksum}))

; -1 fst < snd
; 0 eq
; 1 fst > snd
(defn by->size->alpha
  [[a1 cnt1] [a2 cnt2]]
  (compare [cnt2 a1] [cnt1 a2]))

(defn valid-token?
  [{:keys [ciphertext sector-id checksum]}]
  (let [top-5 (->>
                (clojure.string/replace ciphertext #"-" "")
                frequencies
                seq
                (sort by->size->alpha)
                (take 5))]
    (= (vec checksum) (map first top-5))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> *in*
      slurp
      clojure.string/split-lines
      (map parse-token)
      (filter valid-token?)
      (map :sector-id)
      (reduce +)
      println))
