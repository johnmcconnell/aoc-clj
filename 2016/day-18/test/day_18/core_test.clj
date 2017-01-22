(ns day-18.core-test
  (:require [clojure.test :refer :all]
            [day-18.core :refer :all]))

(deftest next-row-test
  (testing "input: ..^^."
    (is (= ".^^^^" (next-row "..^^."))))

  (testing "input: .^^^^"
    (is (= "^^..^" (next-row ".^^^^")))))

(deftest collect-rows-test
  (testing "input: .^^.^.^^^^"
    (let [expected [".^^.^.^^^^"
                    "^^^...^..^"
                    "^.^^.^.^^."
                    "..^^...^^^"
                    ".^^^^.^^.^"
                    "^^..^.^^.."
                    "^^^^..^^^."
                    "^..^^^^.^^"
                    ".^^^..^.^^"
                    "^^.^^^..^^"]]
      (is (= expected (collect-rows 10 ".^^.^.^^^^"))))))

(deftest count-safe-test
  (testing "input: ..^^."
    (is (= 6 (count-safe
               (collect-rows 3 "..^^."))))))
