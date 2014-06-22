(defproject ambition "0.1.0-SNAPSHOT"
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/cljx"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clj-time "0.6.0"]]

  :main ambition.deathmatch


  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  :profiles {:dev {:plugins [[lein-cljsbuild "1.0.2"]
                             [org.clojure/clojurescript "0.0-2173"]
                             [com.keminglabs/cljx "0.4.0"]]}}

  :cljsbuild {:builds
              [{:id "ambition"
                :source-paths ["target/classes"]
                :compiler {:output-to "ambition.js"
                           :output-dir "out"
                           :optimizations :none
                           :source-map true}}]})
