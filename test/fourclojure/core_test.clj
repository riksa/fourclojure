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
