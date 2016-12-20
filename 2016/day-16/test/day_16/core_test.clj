(ns day-16.core-test
  (:require [clojure.test :refer :all]
            [day-16.core :refer :all]))

(deftest dragon-test
  (testing "1"
    (is (= "100" (dragon "1"))))

  (testing "0"
    (is (= "001" (dragon "0"))))

  (testing "11111"
    (is (= "11111000000" (dragon "11111"))))

  (testing "111100001010"
    (is
      (=
       "1111000010100101011110000"
       (dragon "111100001010")))))

(deftest checksum-test
  (testing "110010110100"
    (is (= "100" (checksum "110010110100")))))

(deftest fill-test
  (testing "example use case"
    (is (= "01100" (fill "10000" 20)))))
