(ns day-6.core-test
  (:require [clojure.test :refer :all]
            [day-6.core :refer :all]))

(deftest corrected-message-test
  (testing "given a corrupted input"
    (let [input (clojure.string/join
                  "\n"
                  ["eedadn"
                   "drvtee"
                   "eandsr"
                   "raavrd"
                   "atevrs"
                   "tsrnev"
                   "sdttsa"
                   "rasrtv"
                   "nssdts"
                   "ntnada"
                   "svetve"
                   "tesnvt"
                   "vntsnd"
                   "vrdear"
                   "dvrsen"
                   "enarar"])]
      (is (= "easter" (corrected-message input))))))
