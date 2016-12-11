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

(deftest decompress-2-test
  (testing "(3x3)XYZ"
    (is (= 9 (decompress-2 "(3x3)XYZ"))))

  (testing "X(8x2)(3x3)ABCY"
    (is (= 20 (decompress-2 "X(8x2)(3x3)ABCY"))))

  (testing "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    (is (= 241920 (decompress-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A"))))

  (testing "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    (is (= 445 (decompress-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")))))
