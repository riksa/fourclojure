(ns fourclojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn p26
	"Write a function which returns the first X fibonacci numbers."
	( [x] (p26 [1 1] x))
	( [s x] 
		(loop [seq s num x]
			(if (< num 3) seq
				(recur (conj seq (reduce + (take-last 2 seq))) (- num 1))

		))))


