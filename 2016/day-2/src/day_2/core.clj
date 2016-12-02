(ns day-2.core
  (:gen-class))

(def up->
  {1 1 2 2 3 3
   4 1 5 2 6 3
   7 4 8 5 9 6})

(def down->
  {1 4 2 5 3 6
   4 7 5 6 6 9
   7 7 8 8 9 9})

(def left->
  {1 1 2 1 3 2
   4 1 5 4 6 5
   7 1 8 7 9 8})

(def right->
  {1 2 2 3 3 3
   4 5 5 6 6 6
   7 8 8 9 9 9})

(def board
  {:up-> up->
   :down-> down->
   :left-> left->
   :right-> right->})

;    1
;  2 3 4
;5 6 7 8 9
;  A B C
;    D
(def *up->
  {1 1
   2 2 3 1 4 4
   5 5 6 2 7 3 8 4 9 9
   :A 6 :B 7 :C 8
   :D :B})

(def *down->
  {1 3
   2 6 3 7 4 8
   5 5 6 :A 7 :B 8 :C 9 9
   :A :A :B :D :C :C
   :D :D})

;    1
;  2 3 4
;5 6 7 8 9
;  A B C
;    D
(def *left->
  {1 1
   2 2 3 2 4 3
   5 5 6 5 7 6 8 7 9 8
   :A :A :B :A :C :B
   :D :D})

(def *right->
  {1 1
   2 3 3 4 4 4
   5 6 6 7 7 8 8 9 9 9
   :A :B :B :C :C :C
   :D :D})

(def new-board
  {:up-> *up->
   :down-> *down->
   :left-> *left->
   :right-> *right->})

(defn button-move
  [{up-> :up->
    down-> :down->
    left-> :left->
    right-> :right->}
    curr move]
  (case move
    "U" (up-> curr)
    "D" (down-> curr)
    "R" (right-> curr)
    "L" (left-> curr)))

(defn final-button
  [board init & moves]
  (reduce (partial button-move board) init moves))

(defn btn-seq-step
  [board [prev-buttons prev-num] move-str]
  (let [moves (map str move-str)
        button (apply final-button board prev-num moves)
        buttons (conj prev-buttons button)]
    [buttons button]))

(defn button-sequence
  [board moves-str]
  (->>
    moves-str
    clojure.string/split-lines
    (reduce (partial btn-seq-step board) [[] 5])
    first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println
    (button-sequence new-board (clojure.string/join "\n" args))))
