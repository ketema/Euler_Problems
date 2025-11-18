(ns quadratic-primes.core-test
  (:require [clojure.test :refer :all]
            [quadratic-primes.core :refer :all]))

(ns quadratic-primes.core-test
  (:require [clojure.test :refer :all]
            [quadratic-primes.core :refer :all]))

(deftest test-is-prime?
  (testing "prime number identification"
    (is (true? (is-prime? 2)))
    (is (true? (is-prime? 3)))
    (is (true? (is-prime? 5)))
    (is (true? (is-prime? 7)))
    (is (true? (is-prime? 11)))
    (is (true? (is-prime? 41)))
    (is (false? (is-prime? 1)))
    (is (false? (is-prime? 4)))
    (is (false? (is-prime? 6)))
    (is (false? (is-prime? 8)))
    (is (false? (is-prime? 9)))
    (is (false? (is-prime? 10)))))

(deftest test-count-consecutive-primes
  (testing "consecutive prime count"
    (is (= 40 (count-consecutive-primes 1 41)))
    (is (= 80 (count-consecutive-primes -79 1601)))))

(deftest test-find-best-coefficients
  (testing "finding the best coefficients"
    (is (= -41 (find-best-coefficients 2 41)))))



