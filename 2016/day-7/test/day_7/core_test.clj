(ns day-7.core-test
  (:require [clojure.test :refer :all]
            [day-7.core :refer :all]))

(deftest abba?-test
  (testing "using abba"
    (is (= true (abba? "abba"))))

  (testing "using aaaa"
    (is (= false (abba? "aaaa")))))

(deftest supports-tls?-test
  (testing "using abba[mnop]qrst"
    (is (= true (supports-tls? "abba[mnop]qrst"))))

  (testing "using abcd[bddb]xyyx"
    (is (= false (supports-tls? "abcd[bddb]xyyx"))))

  (testing "using ioxxoj[asdfgh]zxcvbn"
    (is (= true (supports-tls? "ioxxoj[asdfgh]zxcvbn"))))

  (testing "using ioxxoj[asdfgh]zxcv[bddb]bn"
    (is (= false (supports-tls? "ioxxoj[asdfgh]zxcv[bddb]bn"))))

  (testing "using aaaa[qwer]tyui"
    (is (= false (supports-tls? "aaaa[qwer]tyui")))))

(deftest aba?-test
  (testing "using aba"
    (is (= true (aba? "aba"))))

  (testing "using aaa"
    (is (= false (aba? "aaa")))))
