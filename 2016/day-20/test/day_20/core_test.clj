(ns day-20.core-test
  (:require [clojure.test :refer :all]
            [day-20.core :refer :all]))

; 482
(deftest low-ip-test
  (testing "basic input"
    (let [bl [[5 8]
              [0 2]
              [4 7]]]
      (is (= 3 (low-ip 0 9 bl)))))

  (testing "basic input"
    (let [bl [[0 999]
              [2 87]
              [1000 2014]]]
      (is (= 2015 (low-ip 0 300000 bl))))))

; 399
(deftest count-ip-test
  (testing "basic input"
    (let [bl [[5 8]
              [0 2]
              [4 7]]]
      (is (= 2 (count-ip 0 9 bl)))))

  (testing "basic input"
    (let [bl [[0 999]
              [2 87]
              [1000 2014]]]
      (is (= 1 (count-ip 0 2015 bl))))))
