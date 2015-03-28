(ns user
  (:require [spyscope.core])
  (:require [vinyasa.lein :as lein])
  (:require [clojure.tools.namespace.repl :refer [refresh]])
  (:require [fourclojure.core :refer :all]))

;http://dev.solita.fi/2014/03/18/pimp-my-repl.html

(defn my-repl-util []
  (println "Hello from 'dev'"))

(defn start
  "Start the application"
  []
  (println "Starting"))

(defn stop
  "Stop the application"
  []
  (println "Stopping"))

(defn reset []
  (stop)
  (refresh :after 'user/start))

(defn leintest []
  (lein/lein test))

(defn retest []
  (stop)
  (refresh :after 'user/leintest)
  1)