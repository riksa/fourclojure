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
  ([p] (apply min (map :sum (reduce
                              (fn [l v]
                                (apply concat (map
                                                #(let [i (:index %1) n (inc i) s (:sum %1)]
                                                  (vector
                                                    (hash-map :index i :sum (+ (nth v i) s))
                                                    (hash-map :index n :sum (+ (nth v n) s))
                                                    )) l)))
                              [{:index 0 :sum (-> p first first)}]
                              (rest p))))))

(defn p43
  "Write a function which reverses the interleave process into x number of subsequences."
  ([s n] (for [x (range n)] (keep-indexed #(if (= x (mod %1 n)) %2) s))))

(defn p88
  "Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items
  belonging to one but not both of the two sets."
  ([s1 s2] (apply hash-set (remove (apply hash-set (filter s1 s2)) (into s1 s2)))))

(defn p178
  "Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards. The hand
  rankings are listed below for your convenience.

Straight flush: All cards in the same suit, and in sequence
Four of a kind: Four of the cards have the same rank
Full House: Three cards of one rank, the other two of another rank
Flush: All cards in the same suit
Straight: All cards in sequence (aces can be high or low, but not both at once)
Three of a kind: Three of the cards have the same rank
Two pair: Two pairs of cards have the same rank
Pair: Two cards have the same rank
High card: None of the above conditions are met"
  ([h]
   (letfn [
           (mapcards [s] (map #(hash-map :rank ((zipmap "AKQJT98765432" (iterate dec 14)) (second %)) :suit (first %)) s))
           (flush? [s] (if (->> s mapcards (map :suit) set count (= 1)) :flush))
           (straight? [s] (let [cards (mapcards s)
                                ranks (map :rank cards)
                                min (apply min ranks)]
                            (if (or
                                  (every? (set ranks) (range min (+ 5 min)))
                                  (every? (set ranks) (conj (range 2 6) 14)))
                              :straight)))
           (straight-flush? [s] (if (and (straight? s) (flush? s)) :straight-flush))
           (sets [s] (let [cards (mapcards s)
                           ranks (map :rank cards)]
                       (map count (partition-by identity (sort ranks)))))
           (pair? [s] (if (every? (set (sets s)) [2]) :pair))
           (two-pair? [s] (if (= 2 (count (filter #(= 2 %) (sets s)))) :two-pair))
           (three-of-a-kind? [s] (if (every? (set (sets s)) [3]) :three-of-a-kind))
           (full-house? [s] (if (every? (set (sets s)) [3 2]) :full-house))
           (four-of-a-kind? [s] (if (every? (set (sets s)) [4]) :four-of-a-kind))
           ]
     (or
       (straight-flush? h)
       (four-of-a-kind? h)
       (full-house? h)
       (flush? h)
       (straight? h)
       (three-of-a-kind? h)
       (two-pair? h)
       (pair? h)
       :high-card))))

(defn p128
  "A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - and thirteen cards
  in each suit. Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.

It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five and diamond
queen respectively. But these forms are not convenient for programmers, so to write a card game you need some way to
parse an input string into meaningful components. For purposes of determining rank, we will define the cards to be
valued from 0 (the two) to 12 (the ace)

Write a function which converts (for example) the string 'SJ' into a map of {:suit :spade, :rank 9}. A ten will always
be represented with the single character 'T', rather than the two characters '10'."
  ([c] (hash-map :rank ((zipmap "AKQJT98765432" (iterate dec 12)) (second c))
                 :suit ({'\C :club '\D :diamond '\H :heart '\S :spade} (first c)))))

(defn p135
  "Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write
   a function that does math using the infix notation. Is your favorite language that flexible, Joe? Write a function
   that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /.
   Assume a simple calculator that does not do precedence and instead just calculates left to right."
  ([a o b & r] (let [c (o a b) [x y & z] r] (if r (recur c x y z) c))))

(defn p157
  "Transform a sequence into a sequence of pairs containing the original elements along with their index."
  ([s] (map #(vector %1 %2) s (range))))

(defn p158
  "Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments.
  You may wish to read this."
  ([f] (fn ([& r] (reduce #(%1 %2) f r)))))

(defn p97
  "Pascal's triangle is a triangle of numbers computed using the following rules:
- The first row is 1.
- Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning
and end of the row.
Write a function which returns the nth row of Pascal's Triangle. "
  ([n] (if (= 1 n) [1]
                   (let [p (p97 (dec n))]
                     (vec (map + (conj p 0) (into [0] p)))))))

(defn p118
  "Map is one of the core elements of a functional programming language. Given a function f and an input sequence s,
  return a lazy sequence of (f x) for each element x in s."
  ([f s] (let [[o & r] s] (if (nil? r) (lazy-seq (cons (f o) nil)) (lazy-seq (cons (f o) (p118 f r)))))))

(defn p120
  "Write a function which takes a collection of integers as an argument. Return the count of how many elements are
  smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared;
  whereas 15 is smaller than 1 squared plus 5 squared."
  ([s] (letfn [(d [x] (loop [v [] n x] (let [m (mod n 10) r (/ (- n m) 10)] (if (= 0 r) (cons m v) (recur (cons m v) r)))))
               (z [c] (reduce + (map #(* % %) c)))]
         (count (filter #(< % (z (d %))) s)))))

(defn p50
  "Write a function which takes a sequence consisting of items with different types and splits them up into a set of
  homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences
  themselves can be returned in any order (this is why 'set' is used in the test cases)."
  ([s] (vals (reduce #(assoc %1 (type %2) (cons %2 (%1 (type %2)))) {} (reverse s)))))

(defn p153
  "Given a set of sets, create a function which returns true if no two of those sets have any elements in common1 and
  false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them.

  1Such sets are usually called pairwise disjoint or mutually disjoint."
  ([s] (= (count (reduce into s)) (reduce + (map count s)))))

(defn p55
  "Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
  ([s] (->> s (group-by identity) (reduce-kv #(assoc %1 %2 (count %3)) {}))))

(defn p56
  "Write a function which removes the duplicates from a sequence. Order of the items must be maintained."
  ([s] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] s)))

