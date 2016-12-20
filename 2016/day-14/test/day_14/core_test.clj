(ns day-14.core-test
  (:require [clojure.test :refer :all]
            [day-14.core :refer :all]))

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
    (let [kys #{3}
          triples {"d" #{23}
                   "q" #{1}
                   "i" #{5 23}
                   "z" #{20 23}}
          idx 1013
          hsh "ajhfqqqqqqadddooeiiiiinnzzzzzzakjfk"
          expected #{3 20 23}]
      (is (= expected (conj-kys-of kys triples idx hsh))))))

(deftest keys-64th-idx-test
  (testing "using basic input"
    (is (= 22728 (keys-64th-idx "abc")))))

(deftest stretch-hash-test
  (testing "basic"
    (is (= "a107ff634856bb300138cac6568c0f24" (stretch-hash "abc" 0)))))

(deftest keys-2-64th-idx-test
  (testing "using basic input"
    (is (= 22551 (keys-2-64th-idx "abc")))))
