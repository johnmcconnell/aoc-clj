(ns day-8.core-test
  (:require [clojure.test :refer :all]
            [day-8.core :refer :all]))

(deftest rect-a-b-test
  (testing "turns on those rectangles"
    (let [input (fill-mx 0 2 3)
          expected [[1 0 0]
                    [1 0 0]]]
          (is (= expected (rect-a-b input "2x1"))))))
