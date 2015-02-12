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
  ([s] (reduce #(if true %2 %1) s)))