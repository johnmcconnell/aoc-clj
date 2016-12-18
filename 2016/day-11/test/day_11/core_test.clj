(ns day-11.core-test
  (:require [clojure.test :refer :all]
            [day-11.core :refer :all]))

(deftest pairs-test
  (testing "using (0, 1, 2)"
    (let [input  [0 1 2]
          expected #{[0 1]
                     [0 2]
                     [1 2]}]
      (is (= expected (pairs input)))))

  (testing "on a single pair returns itself"
    (let [input  [0 1]
          expected #{[0 1]}]
      (is (= expected (pairs input)))))

  (testing "doesn't work on less than 2 in coll"
    (let [input  [0]
          expected #{}]
      (is (= expected (pairs input))))))

(deftest valid-state?-test
  (testing "using a valid state"
    (let [input {4 #{}
                 3 #{"L-G"}
                 2 #{"H-G"}
                 1 #{"E" "H-M" "L-M"}}]
      (is (= true (valid-state? input)))))

  (testing "wrong generator with chip"
    (let [input {4 #{}
                 3 #{"L-G"}
                 2 #{"H-G" "L-M"}
                 1 #{"E" "H-M"}}]
      (is (= false (valid-state? input)))))

  (testing "chip protected by generator"
    (let [input {4 #{}
                 3 #{}
                 2 #{"H-G" "H-M" "L-G"}
                 1 #{"E" "L-M"}}]
      (is (= true (valid-state? input))))))

(deftest min-steps-test
  (testing "using sample 1"
    (let [input {4 #{}
                 3 #{"L-G"}
                 2 #{"H-G"}
                 1 #{"E" "H-M" "L-M"}}]
      (is (= 11 (min-steps input))))))

(deftest next-valid-states-test
  (testing "basic next moves"
    (let [input {4 #{}
                 3 #{"L-G"}
                 2 #{"H-G"}
                 1 #{"E" "H-M" "L-M"}}
          expected #{{4 #{}
                      3 #{"L-G"}
                      2 #{"E" "H-G" "H-M"}
                      1 #{"L-M"}}}]
      (is (= expected (next-valid-states #{} input))))))
