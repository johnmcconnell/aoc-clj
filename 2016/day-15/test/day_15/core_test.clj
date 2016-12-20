(ns day-15.core-test
  (:require [clojure.test :refer :all]
            [day-15.core :refer :all]))

(deftest first-pass-test
  (testing "using sample input"
    (let [input [{:t 0 :idx 4 :size 5}
                 {:t 0 :idx 1 :size 2}]]
      (is (= 5 (first-pass input))))))
