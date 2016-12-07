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

(deftest bab?-test
  (testing "using aba"
    (is (= true (bab? "bab" "aba"))))

  (testing "using aaa"
    (is (= false (bab? "cac" "aba")))))

(deftest supports-ssl?-test
  (testing "example 1"
    (is (= true (supports-ssl? "aba[bab]xyz"))))

  (testing "example 2"
    (is (= false (supports-ssl? "xyx[xyx]xyx"))))

  (testing "example 3"
    (is (= true (supports-ssl? "aaa[kek]eke"))))

  (testing "example 4"
    (is (= true (supports-ssl? "zazbz[bzb]cdb")))))

