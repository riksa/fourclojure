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
                            (recur (conj resp (first (split-with (partial = (first seq)) seq))) (last (split-with (partial = (first seq)) seq)))))))

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
  "Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
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
  "The -> macro threads an expression x through a variable number of forms. First, x is inserted as the second item in the first form, making a list of it if it is not a list already. Then the first form is inserted as the second item in the second form, making a list of that form if necessary. This process continues for all the forms. Using -> can sometimes make your code more readable"
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
  "For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work). Write a function that takes three arguments, a less than operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between x and y are as follows:
x = y → :eq
x > y → :gt
x < y → :lt"
  ([o a b] (if (o a b) :lt (if (o b a) :gt :eq))))

(defn p81
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common."
  ([s1 s2] (set (filter #(some #{%} s2) s1))))

(defn p134
  "Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil."
  ([k s] (and (contains? s k) (nil? (k s)))))

(defn p156
  "When retrieving values from a map, you can specify default values in case the key is not found:
(= 2 (:foo {:bar 0, :baz 1} 2))
However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map."
  ([default keys] (reduce #(assoc %1 %2 default) {} keys)))

(defn p62
  "Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc."
  ([f x] (cons x (lazy-seq (p62 f (f x))))))

(defn p107
  "Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called lexical closures. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.

It can be hard to follow in the (abstract, so let's build a simple closure. Given a positive integer n, return a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined."
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