(ns day-12.core-test
  (:require [clojure.test :refer :all]
            [day-12.core :refer :all]))

(deftest parse-test
  (testing "using a value copy"
    (let [state {}
          out {:a 41}]
      (is (= out ((parse "cpy 41 a") state)))))

  (testing "using a register copy"
    (let [state {:b 23}
          out {:a 23
               :b 23}]
      (is (= out ((parse "cpy b a") state)))))

  (testing "using an inc"
    (let [state {:c 15}
          out  {:c 16}]
      (is (= out ((parse "inc c") state)))))

  (testing "using a dec"
    (let [state {:c 15}
          out  {:c 14}]
      (is (= out ((parse "dec c") state)))))

  (testing "using jnz with zero value"
    (let [state {:stack_ptr 0
                 :stack []
                 :c 15}
          out  {:stack_ptr 0
                :stack []
                :c 15}]
      (is (= out ((parse "jnz 0 3") state)))))

  (testing "using jnz with non-zero value"
    (let [state {:stack_ptr 0
                 :stack []
                 :c 15}
          out  {:stack_ptr 2
                :stack []
                :c 15}]
      (is (= out ((parse "jnz 1 3") state)))))

  (testing "using jnz with zero reg"
    (let [state {:stack_ptr 0
                 :stack []
                 :c 0}
          out  {:stack_ptr 0
                :stack []
                :c 0}]
      (is (= out ((parse "jnz c 3") state))))))

(deftest exec-test
  (testing "using example instructions"
    (let [instrs ["cpy 41 a"
                 "inc a"
                 "inc a"
                 "dec a"
                 "jnz a 2"
                 "dec a"]
          expected {:stack_ptr 6
                    :stack instrs
                    :a 42}]
      (is (= expected (exec instrs))))))
