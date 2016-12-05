(ns day-5.core-test
  (:require [clojure.test :refer :all]
            [day-5.core :refer :all]))

(deftest five-zeroes?-test
  (testing "a string has leading 5 zeroes"
    (is (= true (leading-5-0s? "00000abc")))

  (testing "a string does not have leading 5 zeroes"
    (is (= false (leading-5-0s? "00200abc"))))))

(deftest md5-digest-test
  (testing "test a given md5 digent"
    (is (= "00000155f8105dff7f56ee10fa9b9abd" (md5-digest "abc3231929")))))

; this test is expensive
;(deftest password-test
;  (testing "finding a given password"
;    (is (= "18f47a30" (password "abc")))))

; this test is expensive
(deftest password-2-test
  (testing "finding a given password"
    (is (= "05ace8e3" (password-2 "abc")))))
