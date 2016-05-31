(defproject enigma "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [philoskim/debux "0.2.0"]
                 [org.clojure/math.combinatorics "0.1.2"]
                 [perforate "0.3.4"]]
  :plugins [[perforate "0.3.4"]]
  :main ^:skip-aot enigma.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
