(ns day-19.core-test
  (:require [clojure.test :refer :all]
            [day-19.core :refer :all]))

(deftest elf-with-presents-test
  (testing "with four elves"
    (is (= 1 (elf-with-presents (vec (repeat 4 1))))))

  (testing "with five elves"
    (is (= 3 (elf-with-presents (vec (repeat 5 1))))))

  (testing "with six elves"
    (is (= 5 (elf-with-presents (vec (repeat 6 1)))))))
