(ns day-1.core-test
  (:require [clojure.test :refer :all]
            [day-1.core :refer :all]))

(deftest next-direction-test
  (testing "north"
    (is (= :east (next-direction :north :right)))
    (is (= :west (next-direction :north :left))))

  (testing "south"
    (is (= :west (next-direction :south :right)))
    (is (= :east (next-direction :south :left))))

  (testing "east"
    (is (= :south (next-direction :east :right)))
    (is (= :north (next-direction :east :left))))

  (testing "west"
    (is (= :north (next-direction :west :right)))
    (is (= :south (next-direction :west :left)))))

(deftest parse-cmd-test
  (testing "right 2"
    (is (= {:turn :right :count 2} (parse-cmd "R2")))))

(deftest distance-test
  (testing "right 2, left 3"
    (is (= 5 (delta-blocks "R2" "L3"))))

  (testing "right 2, right 2, right 2"
    (is (= 2 (delta-blocks "R2" "R2" "R2"))))

  (testing "right 5, left 5, right 5, right 3"
    (is (= 12 (delta-blocks "R5" "L5" "R5" "R3")))))
