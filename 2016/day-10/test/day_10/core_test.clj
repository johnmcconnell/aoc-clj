(ns day-10.core-test
  (:require [clojure.test :refer :all]
            [day-10.core :refer :all]))

(deftest parse-value-instr-test
  (testing "value 5 goes to bot 2"
    (let [board {:bots {}}
          expected {:bots {2 #{5}}}]
      (is (= expected (run board "value 5 goes to bot 2")))))

  (testing "first high low"
    (let [instrs ["value 5 goes to bot 2"
                  "value 3 goes to bot 1"
                  "value 2 goes to bot 2"
                  "bot 2 gives low to bot 1 and high to bot 0"]
          board {:bots {}}
          expected {:bots {2 #{}
                           1 #{2 3}
                           0 #{5}}}]
      (is (= expected (apply run board instrs)))))

  (testing "all instrs"
    (let [instrs ["value 5 goes to bot 2"
                  "value 3 goes to bot 1"
                  "value 2 goes to bot 2"
                  "bot 2 gives low to bot 1 and high to bot 0"
                  "bot 1 gives low to output 1 and high to bot 0"
                  "bot 0 gives low to output 2 and high to output 0"]
          board {:bots {}}
          expected {:bots {2 #{}
                           1 #{}
                           0 #{}}
                    :outputs {0 #{5}
                              1 #{2}
                              2 #{3}}}]
      (is (= expected (apply run board instrs)))))

  (testing "all instrs out of order"
    (let [instrs ["value 5 goes to bot 2"
                  "bot 2 gives low to bot 1 and high to bot 0"
                  "value 3 goes to bot 1"
                  "bot 1 gives low to output 1 and high to bot 0"
                  "bot 0 gives low to output 2 and high to output 0"
                  "value 2 goes to bot 2"]
          board {:bots {}}
          expected {:bots {2 #{}
                           1 #{}
                           0 #{}}
                    :outputs {0 #{5}
                              1 #{2}
                              2 #{3}}}]
      (is (= expected (apply run board instrs))))))
