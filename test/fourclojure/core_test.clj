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
