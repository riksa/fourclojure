(defproject fourclojure "0.1.0-SNAPSHOT"
            :description "FIXME: write description"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url  "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.6.0"]]
            :main ^:skip-aot fourclojure.core
            :target-path "target/%s"
            :profiles {:uberjar {:aot :all}
                       :dev     {:source-paths ["dev"]
                                 :dependencies [[spyscope "0.1.5"]
                                                [org.clojure/tools.namespace "0.2.9"]
                                                [org.clojure/tools.nrepl "0.2.10"]
                                                [leiningen #=(leiningen.core.main/leiningen-version)]
                                                [im.chit/vinyasa "0.3.4"]]}})
