(ns day-17.core
  (:import
    java.security.MessageDigest
    java.math.BigInteger)
  (:gen-class))

(defn md5
	[s]
	(let [algorithm (MessageDigest/getInstance "MD5")
				size (* 2 (.getDigestLength algorithm))
				raw (.digest algorithm (.getBytes s))
				sig (.toString (BigInteger. 1 raw) 16)
				padding (apply str (repeat (- size (count sig)) "0"))]
		(str padding sig)))

(def valid-chars
  #{\b \c \d \e \f})

(defn open-doors
  [[u d l r]]
  (cond->
    #{}
    (valid-chars u) (conj "U")
    (valid-chars d) (conj "D")
    (valid-chars l) (conj "L")
    (valid-chars r) (conj "R")))

(defn valid-pos
  [[x y]]
  (and
    (> x -1)
    (< x 4)
    (> y -1)
    (< y 4)))

(defn next-moves
  [[x y]]
  (->>
    [[(inc x) y "R"]
     [(dec x) y "L"]
     [x (dec y) "U"]
     [x (inc y) "D"]]
    (filter valid-pos)
    set))

(defn next-paths
  [pwd pos path]
  (let [moves (next-moves pos)
        doors (->> path (apply str pwd) md5 open-doors)]
    (->>
      moves
      (filter #(doors (nth % 2)))
      (map (fn [[x y d]] [[x y] (str path d)]))
      set)))

(defn step
  [pwd]
  (fn
    [nxts]
    (->>
      nxts
      (mapcat
        (fn
          [[pos path]]
          (next-paths pwd pos path))))))

(defn shortest-path
  [pwd]
  (->>
    (iterate (step pwd) [[[0 0] ""]])
    (mapcat identity)
    (drop-while
      #(not= (nth % 0) [3 3]))
    first))

(defn finished-pos?
  [[pos _]]
  (= pos [3 3]))

(defn step-2
  [pwd]
  (fn
    [nxts]
    (->>
      nxts
      (remove finished-pos?)
      (mapcat
        (fn
          [[pos path]]
          (next-paths pwd pos path))))))

(defn longest-path
  [pwd]
  (->>
    (iterate (step-2 pwd) [[[0 0] ""]])
    (take-while #(not (empty? %)))
    (mapcat identity)
    (filter finished-pos?)
    last
    second
    count))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (longest-path "dmypynyp")
    println))
