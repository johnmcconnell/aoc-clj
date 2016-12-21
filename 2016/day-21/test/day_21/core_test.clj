(ns day-21.core-test
  (:require [clojure.test :refer :all]
            [day-21.core :refer :all]))

(deftest rot<-test
  (testing "abcde"
    (is (=
         "cdeab" (apply str ((rot<- ["2" ""]) "abcde"))))))

(deftest rot->test
  (testing "abcde"
    (is (=
         "deabc" (apply str ((rot-> ["2" ""]) "abcde"))))))

(deftest swap-position-test
  (testing "abcde"
    (is (=
         "ebcda" (apply str ((swap-position ["4" nil nil "0"]) "abcde"))))))

(deftest swap-letter-test
  (testing "ebcda"
    (is (=
         "edcba" (apply str ((swap-letter ["d" nil nil "b"]) "ebcda"))))))

(deftest reverse-pos-test
  (testing "edcba"
    (is (=
         "abcde" (apply str ((reverse-pos ["0" nil "4"]) "edcba"))))))

(deftest move-pos-test
  (testing "bcdea"
    (is (=
         "bdeac" (apply str ((move-pos ["1" nil nil "4"]) "bcdea")))))

  (testing "bdeac"
    (is (=
         "abdec" (apply str ((move-pos ["3" nil nil "0"]) "bdeac"))))))

; 333
(deftest scramble-test
  (testing "abcde"
    (let [s "abcde"
          instrs ["swap position 4 with position 0"
                  "swap letter d with letter b"
                  "reverse positions 0 through 4"
                  "rotate left 1 step"
                  "move position 1 to position 4"
                  "move position 3 to position 0"
                  "rotate based on position of letter b"
                  "rotate based on position of letter d"]]
      (is (= "decab" (scramble s instrs))))))

; 309
(deftest unscramble-test
  (testing "decab using letter d"
    (let [s "decab"
          instrs ["rotate based on position of letter d"]]
      (is (= s (unscramble (scramble s instrs) instrs)))))

  (testing "decab using letter b"
    (let [s "decab"
          instrs ["rotate based on position of letter b"]]
      (is (= s (unscramble (scramble s instrs) instrs)))))

  (testing "first 3 instrs")
    (let [s "fbgdceah"
          instrs ["rotate based on position of letter a"
                  "reverse positions 1 through 4"
                  "rotate right 1 step"]]
      ;bgdceahf
      ;becdgahf
      ;
      (is (= "bgdceah" (unscramble s instrs)))))

  (testing "decab"
    (let [s "decab"
          instrs ["swap position 4 with position 0"
                  "swap letter d with letter b"
                  "reverse positions 0 through 4"
                  "rotate left 1 step"
                  "move position 1 to position 4"
                  "move position 3 to position 0"
                  "rotate based on position of letter b"
                  "rotate based on position of letter d"]]
      (is (= "abcde" (unscramble s instrs))))))
