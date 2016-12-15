(ns day-13.core-test
  (:require [clojure.test :refer :all]
            [day-13.core :refer :all]))

(deftest type-of-pos-test
  (testing "0,0 is :open"
    (is (= :open (type-of-pos 0 0 10))))

  (testing "1,0 is :wall"
    (is (= :wall (type-of-pos 1 0 10))))

  (testing "0,1 is :open"
    (is (= :open (type-of-pos 0 1 10))))

  (testing "1,1 is :open"
    (is (= :open (type-of-pos 1 1 10)))))

(deftest next-positions-test
  (testing "gets adjacent"
    (let [[visited x y] [#{} 1 1]
          result #{[0 1] [2 1] [1 0] [1 2]}]
      (is (= result (next-positions visited x y)))))

  (testing "doesn't include visited or self"
    (let [[visited x y] [#{[1 0]} 0 0]
          result #{[0 1]}]
      (is (= result (next-positions visited x y))))))

(deftest next-valid-positions-test
  (testing "gets adjacent"
    (let [[visited x y] [{} 1 1]
          result #{[0 1] [1 2]}]
      (is (= result (next-valid-positions visited 10 x y))))))

(deftest min-dist-test
  (testing "from 1,1 to 1,1"
    (is (= 0 (mindist 10 [1 1] [1 1]))))

  (testing "from 1,1 to 7,4"
    (is (= 11 (mindist 10 [1 1] [7 4])))))
