(ns fourclojure.core-test
  (:require [clojure.test :refer :all]
            [fourclojure.core :refer :all]))

(deftest p26test
  (testing "p26"
    (is
      (= '(1 1 2) (p26 3))
      )
    (is
      (= '(1 1 2 3 5 8) (p26 6))
      )
    (is
      (= '(1 1 2 3 5 8 13 21) (p26 8))
      )
    ))

(deftest p27test
  (testing "p27"
    (is
      (false? (p27 '(1 2 3 4 5)))
      )
    (is
      (true? (p27 "racecar"))
      )
    (is
      (true? (p27 [:foo :bar :foo]))
      )
    (is
      (true? (p27 '(1 1 3 3 1 1)))
      )
    (is
      (false? (p27 '(:a :b :c)))
      )
    ))

(deftest p28test
  (testing "p28"
    (is
      (= (p28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
      )
    (is
      (= (p28 ["a" ["b"] "c"]) '("a" "b" "c"))
      )
    (is
      (= (p28 '((((:a))))) '(:a))
      )
    ))

(deftest p29test
  (testing "p29"
    (is
      (= (p29 "HeLlO, WoRlD!") "HLOWRD")
      )
    (is
      (empty? (p29 "nothing"))
      )
    (is
      (= (p29 "$#A(*&987Zf") "AZ")
      )
    ))

(deftest p30test
  (testing "p30"
    (is
      (= (apply str (p30 "Leeeeeerrroyyy")) "Leroy")
      )
    (is
      (= (p30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
      )
    (is
      (= (p30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
      )
    ))

(deftest p31test
  (testing "p31"
    (is
      (= (p31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
      )
    (is
      (= (p31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
      )
    (is
      (= (p31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
      )
    ))

(deftest p32test
  (testing "p32"
    (is
      (= (p32 [1 2 3]) '(1 1 2 2 3 3)))
    (is
      (= (p32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
    (is
      (= (p32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
    (is
      (= (p32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
    ))

(deftest p33test
  "Write a function which replicates each element of a sequence a variable number of times."
  (testing "p33"
    (is
      (= (p33 [1 2 3] 2) '(1 1 2 2 3 3)))
    (is
      (= (p33 [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
    (is
      (= (p33 [4 5 6] 1) '(4 5 6)))
    (is
      (= (p33 [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
    (is
      (= (p33 [44 33] 2) [44 44 33 33]))
    ))

(deftest p34test
  "Write a function which creates a list of all integers in a given range."
  (testing "p34"
    (is
      (= (p34 1 4) '(1 2 3)))
    (is
      (= (p34 -2 2) '(-2 -1 0 1)))
    (is
      (= (p34 5 8) '(5 6 7)))
    ))