(ns day-1.core-test
  (:require [clojure.test :refer :all]
            [day-1.core :refer :all]))

(deftest a-test
  (testing "test 1"
    (is (= 4 (count-it "1111"))))

  (testing "test 2"
    (is (= 9 (count-it "91212129")))))

(deftest v-test
  (testing "test 1"
    (is (= 0 (count-it-2 "1221"))))

  (testing "test 3"
    (is (= 12 (count-it-2 "123123"))))

  (testing "test 2"
    (is (= 6 (count-it-2 "1212")))))
