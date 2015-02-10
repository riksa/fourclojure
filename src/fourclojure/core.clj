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
	( [x] (p26 [1 1] x))
	( [s x] 
		(loop [seq s num x]
			(if (< num 3) seq
				(recur (conj seq (reduce + (take-last 2 seq))) (- num 1))

		))))

;You tripped the alarm! def is bad!
;replace "(defn" with "(fn" and remove docline "..."
(defn p27
	"Write a function which returns true if the given sequence is a palindrome."
	( [s] (if (string? s) (p27 (apply str (reverse s)) s ) (p27 s (reverse s))))
	( [s r] (= s r)))
