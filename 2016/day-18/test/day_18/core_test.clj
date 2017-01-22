(ns day-18.core-test
  (:require [clojure.test :refer :all]
            [day-18.core :refer :all]))

(deftest next-row-test
  (testing "input: ..^^."
    (is (= ".^^^^" (next-row "..^^."))))

  (testing "input: .^^^^"
    (is (= "^^..^" (next-row ".^^^^")))))

;(deftest next-rows-test
;  (testing "input: .^^.^.^^^^"
;    (let [expected [["^^^...^..^"]
;                    ["^.^^.^.^^."]
;                    ["..^^...^^^"]
;                    [".^^^^.^^.^"]
;                    ["^^..^.^^.."]
;                    ["^^^^..^^^."]
;                    ["^..^^^^.^^"]
;                    [".^^^..^.^^"]
;                    ["^^.^^^..^^"]]]
;      (is (= expected (next-rows ".^^.^.^^^^" 40))))))
