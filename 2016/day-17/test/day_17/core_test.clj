(ns day-17.core-test
  (:require [clojure.test :refer :all]
            [day-17.core :refer :all]))

(deftest open-doors-test
  (testing "test open doors with abcd012219"
    (is (= #{"D" "L" "R"} (open-doors "abcd012219"))))

  (testing "test open doors with ef01hel2219"
    (is (= #{"U" "D"} (open-doors "ef01hel2219")))))

(deftest next-paths-test
  (testing "test next open paths with hijkl [0 0] []"
    (is (= #{[[0 1] "D"]} (next-paths "hijkl" [0 0] ""))))

  (testing "test next open paths with hijkl [0 0] []"
    (is (= #{[[0 0] "DU"]
             [[1 1] "DR"]} (next-paths "hijkl" [0 1] "D")))))

(deftest shortest-path-test
  (testing "testing: ihgpwlah"
    (is
      (=
       [[3 3] "DDRRRD"]
       (shortest-path "ihgpwlah"))))

  (testing "testing: kglvqrro"
    (is
      (=
       [[3 3] "DDUDRLRRUDRD"]
       (shortest-path "kglvqrro"))))

  (testing "testing: ulqzkmiv"
    (is
      (=
       [[3 3] "DRURDRUDDLLDLUURRDULRLDUUDDDRR"]
       (shortest-path "ulqzkmiv")))))

(deftest longest-path-test
  (testing "testing: ihgpwlah"
    (is
      (=
       370
       (longest-path "ihgpwlah"))))

  (testing "testing: kglvqrro"
    (is
      (=
       492
       (longest-path "kglvqrro"))))

  (testing "testing: ulqzkmiv"
    (is
      (=
       830
       (longest-path "ulqzkmiv")))))
