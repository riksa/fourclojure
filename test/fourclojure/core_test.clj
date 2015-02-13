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

(deftest p35test
  "Clojure lets you give local names to values using the special let-form."
  (testing "p35"
    (is
      (= (p35) (let [x 5] (+ 2 x))))
    (is
      (= (p35) (let [x 3, y 10] (- y x))))
    (is
      (= (p35) (let [x 21] (let [y 3] (/ x y)))))
    ))

(deftest p38test
  (testing "p38"
    (is
      (= (p38 1 8 3 4) 8))
    (is
      (= (p38 30 20) 30))
    (is
      (= (p38 45 67 11) 67))
    ))

(deftest p39test
  (testing "p39"
    (is
      (= (p39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
    (is
      (= (p39 [1 2] [3 4 5 6]) '(1 3 2 4)))
    (is
      (= (p39 [1 2 3 4] [5]) [1 5]))
    (is
      (= (p39 [30 20] [25 15]) [30 25 20 15]))
    ))

(deftest p40test
  (testing "p40"
    (is (= (p40 0 [1 2 3]) [1 0 2 0 3]))
    (is (= (apply str (p40 ", " ["one" "two" "three"])) "one, two, three"))
    (is (= (p40 :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
    ))

(deftest p41test
  (testing "p41"
    (is (= (p41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
    (is (= (p41 [:a :b :c :d :e :f] 2) [:a :c :e]))
    (is (= (p41 [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

(deftest p42test
  (testing "42"
    (is (= (p42 1) 1))
    (is (= (p42 3) 6))
    (is (= (p42 5) 120))
    (is (= (p42 8) 40320))))

(deftest p71test
  (testing "p71"
    (is (= (p71 (sort (rest (reverse [2 5 4 1 3 6]))))
           (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (p71))
           5))))

(deftest p19test
  (testing "p19"
    (is (= (p19 [1 2 3 4 5]) 5))
    (is (= (p19 '(5 4 3)) 3))
    (is (= (p19 ["b" "c" "d"]) "d"))))

(deftest p49test
  (testing "p49"
    (is (= (p49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
    (is (= (p49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
    (is (= (p49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(deftest p61test
  (testing "p61"
    (is (= (p61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
    (is (= (p61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
    (is (= (p61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

(deftest p66test
  (testing "p66"
    (is (= (p66 2 4) 2))
    (is (= (p66 10 5) 5))
    (is (= (p66 5 7) 1))
    (is (= (p66 1023 858) 33))))