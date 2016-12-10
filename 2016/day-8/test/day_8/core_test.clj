(ns day-8.core-test
  (:require [clojure.test :refer :all]
            [day-8.core :refer :all]))

(deftest rect-test
  (testing "turns on 2x1 rectangles"
    (let [input (fill-mx 0 2 3)
          expected [[1 0 0]
                    [1 0 0]]]
          (is (= expected (rect input 2 1)))))

  (testing "turns on 2x2 rectangles"
    (let [input (fill-mx 0 2 3)
          expected [[1 1 0]
                    [1 1 0]]]
          (is (= expected (rect input 2 2))))))

(deftest rotate-row-test
  (testing "shifts 1 1"
    (let [input [[1 2 3]
                 [4 5 6]
                 [7 8 9]]
          expected [[1 2 3]
                    [6 4 5]
                    [7 8 9]]]
          (is (= expected (rotate-row input 1 1)))))

  (testing "shifts 2 2"
    (let [input [[1 2 3]
                 [4 5 6]
                 [7 8 9]]
          expected [[1 2 3]
                    [4 5 6]
                    [8 9 7]]]
          (is (= expected (rotate-row input 2 2))))))

(deftest rotate-col-test
  (testing "shifts 1 1"
    (let [input [[1 2 3]
                 [4 5 6]
                 [7 8 9]]
          expected [[1 8 3]
                    [4 2 6]
                    [7 5 9]]]
          (is (= expected (rotate-col input 1 1)))))

  (testing "shifts 2 2"
    (let [input [[1 2 3]
                 [4 5 6]
                 [7 8 9]]
          expected [[1 2 6]
                    [4 5 9]
                    [7 8 3]]]
          (is (= expected (rotate-col input 2 2))))))
