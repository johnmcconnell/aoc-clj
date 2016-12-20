(ns day-14.core-test
  (:require [clojure.test :refer :all]
            [day-14.core :refer :all]))

(deftest hashes-test
  (testing "taking first two hashes"
    (let [v (last (take 18 (hashes "abc")))
          expected "0034e0923cc38887a57bd7b1d4f953df"]
      (is (=  expected v)))))

(deftest conj-triples-of-test
  (testing "basic triples"
    (let [triples {"i" #{5}}
          idx 23
          hsh "ajhfddddaooeiiinnzzzzzzakjfk"
          expected {"d" #{23}
                    "i" #{5}}]
      (is (= expected (conj-triples-of triples idx hsh))))))

(deftest conj-kys-of-test
  (testing "complex fivelets"
    (let [kys [["a" 3]]
          triples {"d" #{23}
                   "q" #{1}
                   "i" #{5 23}
                   "z" #{20 23}}
          idx 1013
          hsh "ajhfqqqqqqadddooeiiiiinnzzzzzzakjfk"
          expected [["a" 3]
                    ["i" 1013]
                    ["z" 1013]
                    ["z" 1013]]]
      (is (= expected (conj-kys-of kys triples idx hsh))))))

(deftest keys-64th-idx-test
  (testing "using basic input"
    (is (= 22728 (keys-64th-idx "abc")))))
