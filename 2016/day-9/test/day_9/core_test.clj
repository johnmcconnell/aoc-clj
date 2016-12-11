(ns day-9.core-test
  (:require [clojure.test :refer :all]
            [day-9.core :refer :all]))

(deftest decompress-test
  (testing "ADVENT"
    (is (= "ADVENT" (decompress "ADVENT"))))

  (testing "(3x3)XYZ"
    (is (= "XYZXYZXYZ" (decompress "(3x3)XYZ"))))

  (testing "(6x1)(1x3)A"
    (is (= "(1x3)A" (decompress "(6x1)(1x3)A"))))

  (testing "X(8x2)(3x3)ABCY"
    (is (= "X(3x3)ABC(3x3)ABCY" (decompress "X(8x2)(3x3)ABCY"))))

  (testing "A(1x5)BC"
    (is (= "ABBBBBC" (decompress "A(1x5)BC")))))
