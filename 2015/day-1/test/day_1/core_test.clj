(ns day-1.core-test
  (:require [clojure.test :refer :all]
            [day-1.core :refer :all]))

(deftest understanding-parenthesis
  (testing "open-parens inc floor"
    (let [floor (-> (climb "(((") :floor)]
      (is (= floor 3))))

  (testing "closed-parens dec floor"
    (let [floor (-> (climb ")))") :floor)]
      (is (= floor -3)))))

(deftest finding-basement
  (testing "basement in first position"
    (let [pos (-> (climb ")") :pos)]
      (is (= pos 1))))

  (testing "basement in first position"
    (let [pos (-> (climb "()())") :pos)]
      (is (= pos 5)))))
