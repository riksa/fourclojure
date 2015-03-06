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

(deftest p166test
  (testing "p166"
    (is (= :gt (p166 < 5 1)))
    (is (= :eq (p166 (fn [x y] (< (count x) (count y))) "pear" "plum")))
    (is (= :lt (p166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
    (is (= :gt (p166 > 0 2)))))

(deftest p81test
  (testing "p81"
    (is (= (p81 #{0 1 2 3} #{2 3 4 5}) #{2 3}))
    (is (= (p81 #{0 1 2} #{3 4 5}) #{}))
    (is (= (p81 #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

(deftest p134test
  (testing "p134"
    (is (true? (p134 :a {:a nil :b 2})))
    (is (false? (p134 :b {:a nil :b 2})))
    (is (false? (p134 :c {:a nil :b 2})))))

(deftest p156test
  (testing "p156"
    (is (= (p156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (p156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (p156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

(deftest p62test
  (testing "p62"
    (is (= (take 5 (p62 #(* 2 %) 1)) [1 2 4 8 16]))
    (is (= (take 100 (p62 inc 0)) (take 100 (range))))
    (is (= (take 9 (p62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))))

(deftest p107test
  (testing "p107"
    (is (= 256 ((p107 2) 16),
           ((p107 8) 2)))
    (is (= [1 8 27 64] (map (p107 3) [1 2 3 4])))
    (is (= [1 2 4 8 16] (map #((p107 %) 2) [0 1 2 3 4])))))

(deftest p99test
  (testing "p99"
    (is (= (p99 1 1) [1]))
    (is (= (p99 99 9) [8 9 1]))
    (is (= (p99 999 99) [9 8 9 0 1]))))

(deftest p46test
  (testing "p46"
    (is (= 3 ((p46 nth) 2 [1 2 3 4 5])))
    (is (= true ((p46 >) 7 8)))
    (is (= 4 ((p46 quot) 2 8)))
    (is (= [1 2 3] ((p46 take) [1 2 3 4 5] 3)))))

(deftest p90test
  (testing "p90"
    (is (= (p90 #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
           #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
             ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
             ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
    (is (= (p90 #{1 2 3} #{4 5})
           #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    (is (= 300 (count (p90 (into #{} (range 10))
                           (into #{} (range 30))))))))

(deftest p122test
  (testing "p122"
    (is (= 0 (p122 "0")))
    (is (= 7 (p122 "111")))
    (is (= 8 (p122 "1000")))
    (is (= 9 (p122 "1001")))
    (is (= 255 (p122 "11111111")))
    (is (= 1365 (p122 "10101010101")))
    (is (= 65535 (p122 "1111111111111111")))))

(deftest p63test
  (testing "p63"
    (is (= (p63 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
    (is (= (p63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
           {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
    (is (= (p63 count [[1] [1 2] [3] [1 2 3] [2 3]])
           {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

(deftest p143test
  (testing "p143"
    (is (= 0 (p143 [0 1 0] [1 0 0])))
    (is (= 3 (p143 [1 1 1] [1 1 1])))
    (is (= 32 (p143 [1 2 3] [4 5 6])))
    (is (= 256 (p143 [2 5 6] [100 10 1])))))

(deftest p44test
  (testing "p44"
    (is (= (p44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
    (is (= (p44 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
    (is (= (p44 6 [1 2 3 4 5]) '(2 3 4 5 1)))
    (is (= (p44 1 '(:a :b :c)) '(:b :c :a)))
    (is (= (p44 -4 '(:a :b :c)) '(:c :a :b)))))

(deftest p73test
  (testing "p73"
    (is (= nil (p73 [[:e :e :e]
                     [:e :e :e]
                     [:e :e :e]])))
    (is (= :x (p73 [[:x :e :o]
                    [:x :e :e]
                    [:x :e :o]])))
    (is (= :o (p73 [[:e :x :e]
                    [:o :o :o]
                    [:x :e :x]])))
    (is (= nil (p73 [[:x :e :o]
                     [:x :x :e]
                     [:o :x :o]])))
    (is (= :x (p73 [[:x :e :e]
                    [:o :x :e]
                    [:o :e :x]])))
    (is (= :o (p73 [[:x :e :o]
                    [:x :o :e]
                    [:o :e :x]])))
    (is (= nil (p73 [[:x :o :x]
                     [:x :o :x]
                     [:o :x :o]])))))

(deftest p92test
  (testing "p92"
    (is (= 14 (p92 "XIV")))
    (is (= 827 (p92 "DCCCXXVII")))
    (is (= 3999 (p92 "MMMCMXCIX")))
    (is (= 48 (p92 "XLVIII")))))

(deftest p79test
  (testing "p79"
    (is (= 7 (p79 '([1]
                     [2 4]
                     [5 1 4]
                     [2 3 4 5]))))
    (is (= 20 (p79 '([3]
                      [2 4]
                      [1 9 3]
                      [9 9 2 4]
                      [4 6 6 7 8]
                      [5 7 3 5 1 4]))))))

(deftest p43test
  (testing "p43"
    (is (= (p43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (p43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (p43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest p88test
  (testing "p88"
    (is (= (p88 #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
    (is (= (p88 #{:a :b :c} #{}) #{:a :b :c}))
    (is (= (p88 #{} #{4 5 6}) #{4 5 6}))
    (is (= (p88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))))

(deftest p178test
  (testing "p178"
    (is (= :high-card (p178 ["HA" "D2" "H3" "C9" "DJ"])))
    (is (= :pair (p178 ["HA" "HQ" "SJ" "DA" "HT"])))
    (is (= :two-pair (p178 ["HA" "DA" "HQ" "SQ" "HT"])))
    (is (= :three-of-a-kind (p178 ["HA" "DA" "CA" "HJ" "HT"])))
    (is (= :straight (p178 ["HA" "DK" "HQ" "HJ" "HT"])))
    (is (= :straight (p178 ["HA" "H2" "S3" "D4" "C5"])))
    (is (= :flush (p178 ["HA" "HK" "H2" "H4" "HT"])))
    (is (= :full-house (p178 ["HA" "DA" "CA" "HJ" "DJ"])))
    (is (= :four-of-a-kind (p178 ["HA" "DA" "CA" "SA" "DJ"])))
    (is (= :straight-flush (p178 ["HA" "HK" "HQ" "HJ" "HT"])))))

(deftest p128test
  (testing "p128"
    (is (= {:suit :diamond :rank 10} (p128 "DQ")))
    (is (= {:suit :heart :rank 3} (p128 "H5")))
    (is (= {:suit :club :rank 12} (p128 "CA")))
    (is (= (range 13) (map (comp :rank p128 str)
                           '[S2 S3 S4 S5 S6 S7
                             S8 S9 ST SJ SQ SK SA])))))


(deftest p135test
  (testing "p135"
    (is (= 7  (p135 2 + 5)))
    (is (= 42 (p135 38 + 48 - 2 / 2)))
    (is (= 8  (p135 10 / 2 - 1 * 2)))
    (is (= 72 (p135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))))