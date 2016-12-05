(ns day-3.core-test
  (:require [clojure.test :refer :all]
            [day-3.core :refer :all]))

(deftest valid-triangle-test
  (testing (str "the length of two sides "
                "is greater than the opposite side "
                "for every side")
    (is (= true (valid-triangle? [1 2 2]))))

  (testing (str "the length of two sides "
                "is not greater than the opposite side")
    (is (= false (valid-triangle? [1 2 6])))))

(deftest parse-triangle-test
  (testing "it can parse a triangle"
    (is (= [1 2 3] (parse-triangle "  1   2    3  ")))))
