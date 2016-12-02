(ns day-2.core-test
  (:require [clojure.test :refer :all]
            [day-2.core :refer :all]))

(deftest final-button-test
  (testing "the initial sequence ULL"
    (is (= 1 (final-button 5 "U" "L" "L")))))
