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
