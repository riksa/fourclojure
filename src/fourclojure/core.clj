(ns fourclojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;You tripped the alarm! def is bad!
;replace "(defn" with "(fn" and remove docline "..."
(defn p26
  "Write a function which returns the first X fibonacci numbers."
  ([x] (p26 [1 1] x))
  ([s x]
   (loop [seq s num x]
     (if (< num 3) seq
                   (recur (conj seq (reduce + (take-last 2 seq))) (- num 1))

                   ))))

;You tripped the alarm! def is bad!
;replace "(defn" with "(fn" and remove docline "..."
(defn p27
  "Write a function which returns true if the given sequence is a palindrome."
  ([s] (if (string? s) (p27 (apply str (reverse s)) s) (p27 s (reverse s))))
  ([s r] (= s r)))

(defn p28
  "Write a function which flattens a sequence."
  ([seq] (if (coll? seq) (reduce concat (map p28 seq)) (list seq))))

(defn p29
  "Write a function which takes a string and returns a new string containing only the capital letters."
  ([s] (apply str (filter #(and (<= (int %) (int \Z)) (>= (int %) (int \A))) s))))

(defn p30
  "Write a function which removes consecutive duplicates from a sequence."
  ([s] (reduce p30 [] s))
  ([a b] (if (= (last a) b) a (conj a b))))

(defn p31
  "Write a function which packs consecutive duplicates into sub-lists."
  ([s] (p31 [] s))
  ([r s] (loop [resp r seq s]
           (if (empty? seq) resp
                            (recur (conj resp (first (split-with (partial = (first seq)) seq)))
                                   (last (split-with (partial = (first seq)) seq)))))))

(defn p32
  "Write a function which duplicates each element of a sequence."
  ([s] (interleave s s)))

(defn p33
  "Write a function which replicates each element of a sequence a variable number of times."
  ([s n] (apply concat (map #(repeat n %) s))))

(defn p34
  "Write a function which creates a list of all integers in a given range."
  ([a b] (take (- b a) (iterate inc a))))

(defn p35
  "Clojure lets you give local names to values using the special let-form."
  ([] 7))

; p36 and p37 were not really testable, just teaching to assign

(defn p38
  "Write a function which takes a variable number of parameters and returns the maximum value."
  ([& rest] (reduce #(if (> %1 %2) %1 %2) rest)))

(defn p39
  "Write a function which takes two sequences and returns the first item from each, then the second item from each, then
  the third, etc."
  ([s1 s2] (loop [r [] s1 s1 s2 s2]
             (if (or (empty? s1) (empty? s2)) r (recur (conj r (first s1) (first s2)) (rest s1) (rest s2))))))

(defn p40
  "Write a function which separates the items of a sequence by an arbitrary value."
  ([v s] (loop [r [] s1 s]
           (if (empty? s1) (drop-last r) (recur (conj r (first s1) v) (rest s1))))))

(defn p41
  "Write a function which drops every Nth item from a sequence."
  ([s n] (loop [r [] s s]
           (if (empty? s) r (recur (concat r (take (dec n) s)) (drop n s))))))

(defn p42
  "Write a function which calculates factorials."
  ([n] (reduce * (take n (iterate inc 1)))))

(defn p71
  "The -> macro threads an expression x through a variable number of forms. First, x is inserted as the second item in
  the first form, making a list of it if it is not a list already. Then the first form is inserted as the second item in
  the second form, making a list of that form if necessary. This process continues for all the forms. Using -> can
  sometimes make your code more readable"
  ([s] (last s)))

(defn p19
  "Write a function which returns the last element in a sequence."
  ([s] (-> s reverse first)))

(defn p49
  "Write a function which will split a sequence into two parts."
  ([n s] [(take n s) (drop n s)]))

(defn p61
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  ([k v] (loop [s {} k k v v]
           (if (or (empty? v) (empty? k)) s
                                          (recur (assoc s (first k) (first v)) (rest k) (rest v))))))

(defn p66
  "Given two integers, write a function which returns the greatest common divisor."
  ([a b] (some #(if (and (= 0 (mod a %)) (= 0 (mod b %))) %) (iterate dec (min a b)))))

(defn p166
  "For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >)
  from a single operation (any operator but = or ≠ will work). Write a function that takes three arguments, a less than
  operator for the data and two items to compare. The function should return a keyword describing the relationship
  between the two items. The keywords for the relationship between x and y are as follows:
x = y → :eq
x > y → :gt
x < y → :lt"
  ([o a b] (if (o a b) :lt (if (o b a) :gt :eq))))

(defn p81
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set
  has in common."
  ([s1 s2] (set (filter #(some #{%} s2) s1))))

(defn p134
  "Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value
  is nil."
  ([k s] (and (contains? s k) (nil? (k s)))))

(defn p156
  "When retrieving values from a map, you can specify default values in case the key is not found:
(= 2 (:foo {:bar 0, :baz 1} 2))
However, what if you want the map itself to contain the default values? Write a function which takes a default value and
 a sequence of keys and constructs a map."
  ([default keys] (reduce #(assoc %1 %2 default) {} keys)))

(defn p62
  "Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence
  of x, (f x), (f (f x)), (f (f (f x))), etc."
  ([f x] (cons x (lazy-seq (p62 f (f x))))))

(defn p107
  "Lexical scope and first-class functions are two of the most basic building blocks of a functional language like
  Clojure. When you combine the two together, you get something very powerful called lexical closures. With these, you
  can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long
  after the code you're running now has finished.

It can be hard to follow in the (abstract, so let's build a simple closure. Given a positive integer n, return a
function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside the
scope in which it is defined."
  [n] #(int (Math/pow % n)))


(defn p99
  "Write a function which multiplies two numbers and returns the result as a sequence of its digits."
  ([n] (loop [r '() n n]
         (let [mod (mod n 10) rem (- n mod)]
           (if (and (= 0 mod) (= 0 rem)) r (recur (conj r mod) (/ rem 10))))))
  ([a b] (p99 (* a b))))

(defn p46
  "Write a higher-order function which flips the order of the arguments of an input function."
  ([f] #(f %2 %1)))

(defn p90
  "Write a function which calculates the Cartesian product of two sets."
  ([s1 s2] (into #{} (apply concat (map
                                     (fn [a]
                                       (map #(vec [%1 a]) s1)) s2)))))

;#spy/p
(defn p122
  "Convert a binary number, provided in the form of a string, to its numerical value."
  ([s] (reduce + (map #(if (= '\1 %1) %2 0) (reverse s) (iterate (partial * 2) 1)))))

(defn p63
  "Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied
   to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s."
  ([f s] (reduce
           #(let [k (f %2) v %2 c (or (%1 k) [])]
             (assoc %1 k (conj c v))) {} s)))

(defn p143
  "Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same
  length."
  ([s1 s2] (reduce + (map * s1 s2))))

(defn p44
  "Write a function which can rotate a sequence in either direction."
  ([o s] (let [o (mod o (count s))] (concat (drop o s) (take o s)))))

(defn p73
  "A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and
  empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row.
  Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither
  player has won."
  ([b] (letfn [(w [c] (let [s (set c)] (if (= 1 (count s)) (some #{:o :x} s))))]
         (some #{:x :o} (set (map w (concat
                                           b
                                           (apply map vector b)
                                           [(map #(%1 %2) b (iterate dec 2))
                                            (map #(%1 %2) b (iterate inc 0))]
                                           )))))))

(defn p92
  "Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. Write a
  function to parse a Roman-numeral string and return the number it represents.

You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle. You don't need
to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters."
  ([s] (->> s
            reverse
            (map {'\M 1000 '\D 500 '\C 100 '\L 50 '\X 10 '\V 5 '\I 1})
            (reduce #(let [o (if (< %2 (or (:last %1) 0)) - +)]
                      (hash-map :sum (o (or (:sum %1) 0) %2) :last %2)) '{})
            :sum)))

(defn p79
  "Write a function which calculates the sum of the minimal path through a triangle. The triangle is represented as a
  collection of vectors. The path should start at the top of the triangle and move to an adjacent number on the next row
  until the bottom of the triangle is reached."
  ([v] (reduce #() '() v)))

; (0) (0 1) (1 2) (2 3)
; (iterate #(vector (inc (first %1)) (inc (second %1))) [-1 0])

(def p '([1]
          [2 4]
          [5 1 4]
          [2 3 4 5]))

(def v1 (first p))
(def v2 (second p))
(def v3 (nth p 2))
(def v4 (nth p 3))

(def indexes (iterate #(vector (inc (first %1)) (inc (second %1))) [-1 0]))

(defn splitter
  ([v1 v2] (let [indexes (iterate #(vector (inc (first %1)) (inc (second %1))) [-1 0])]
         ())))

(defn combiner
  ([c v]
   (into [] (flatten
              (filter #(not (some nil? %))
                      (let [o1 (conj c [nil]) o2 (into [[nil]] c) _ #spy/p o1 _ #spy/p o2 _ #spy/p v] (for [i (range (count v))] (vector (into (nth o1 i) v) (into (nth o2 i) v))))
                      )
                 )

;   (concat #spy/p(map #(conj %2 %1) v (conj c [nil])) #spy/p(map #(conj %2 %1) v (into [[nil]] c)) ))
;           )
    )))

(defn combiner
  ([c v] (into []
               (filter #(not (some nil? %))
               (apply concat
                 (let [cyc (cycle (into [[nil]] c))]
                   (for [i (range (count v))] (vector (conj (nth cyc i) (nth v i)) (conj (nth cyc (inc i)) (nth v i)))))
                 )     ))))

(defn reducer
  ([p] (apply min (map #(reduce + %) (reduce combiner (vector (first p)) (rest p)))) ))

;(reduce combiner (vector (first p)) (rest p))


;(apply min (map #(reduce + (flatten %)) (combiner (combiner (combiner (vector (map vector v1)) v2) v3) v4)))

;1

;1 1
;2 4

;1 1 1 1
;2 2 4 4
;5 1 1 4

; (filter #(not (some nil? %)) (concat (map vector v3 (conj v2 nil)) (map vector v3 (into [nil] v2))))

;5 5 1 1 1 1 4 4
;2 3 3 4 3 4 4 5
; take a row
;