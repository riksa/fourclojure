(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]])
  (:require [fourclojure.core :refer :all]))

;http://dev.solita.fi/2014/03/18/pimp-my-repl.html

(defn my-repl-util []
  (println "Hello from 'dev'"))

(defn start
  "Start the application"
  []
  nil)

(defn stop
  "Stop the application"
  []
  nil)

(defn reset []
  (stop)
  (refresh :after 'user/start))