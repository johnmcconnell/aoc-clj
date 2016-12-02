(ns day-2.core-test
  (:require [clojure.test :refer :all]
            [day-2.core :refer :all]))

(deftest final-button-test
  (testing "the initial sequence ULL"
    (is (= 1 (final-button board 5 "U" "L" "L"))))

  (testing "the next sequence RRDDD"
    (is (= 9 (final-button board 1 "R" "R" "D" "D" "D"))))

  (testing "the third sequence LURDL"
    (is (= 8 (final-button board 9 "L" "U" "R" "D" "L"))))

  (testing "the fourth sequence UUUUD"
    (is (= 5 (final-button board 5 "U" "U" "U" "U" "D")))))

(deftest button-sequence-test
  (testing "the initial sequence ULL RRDDD"
    (let [cmd-str "ULL\nRRDDD"]
      (is (= [1 9] (button-sequence board cmd-str))))))

(deftest final-button-test
  (testing "the initial sequence ULL"
    (is (= 5 (final-button new-board 5 "U" "L" "L"))))

  (testing "the next sequence RRDDD"
    (is (= :D (final-button new-board 5 "R" "R" "D" "D" "D"))))

  (testing "the third sequence LURDL"
    (is (= :B (final-button new-board :D "L" "U" "R" "D" "L"))))

  (testing "the fourth sequence UUUUD"
    (is (= 3 (final-button new-board :B "U" "U" "U" "U" "D")))))

(deftest button-sequence-test
  (testing "the initial sequence ULL RRDDD"
    (let [cmd-str "ULL\nRRDDD\nLURDL\nUUUUD"]
      (is (= [5 :D :B 3] (button-sequence new-board cmd-str))))))
