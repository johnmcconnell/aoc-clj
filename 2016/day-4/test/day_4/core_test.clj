(ns day-4.core-test
  (:require [clojure.test :refer :all]
            [day-4.core :refer :all]))

(deftest parse-token-test
  (testing "a sample token"
    (let [input "aaaaa-bbb-z-y-x-123[abxyz]"
          expected {:ciphertext "aaaaa-bbb-z-y-x"
                    :sector-id  123
                    :checksum   "abxyz"}]
      (is (= expected (parse-token input)))))

  (testing "another sample token"
    (let [input "not-a-real-room-404[oarel]"
          expected {:ciphertext "not-a-real-room"
                    :sector-id  404
                    :checksum   "oarel"}]
      (is (= expected (parse-token input))))))

(deftest valid-token?-test
  (testing "a valid token"
    (let [input {:ciphertext "aaaaa-bbb-z-y-x"
                 :sector-id  123
                 :checksum   "abxyz"}
          expected true]
      (is (= expected (valid-token? input)))))

  (testing "an invalid token"
    (let [input {:ciphertext "totally-real-room"
                 :sector-id  200
                 :checksum   "decoy"}
          expected false]
      (is (= expected (valid-token? input))))))
