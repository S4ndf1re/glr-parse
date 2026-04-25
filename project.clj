(defproject glr_parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [com.phronemophobic/clj-graphviz "0.6.4"]]
  :main ^:skip-aot glr-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :jvm-opts ["-Djava.library.path=/opt/homebrew/lib"
             "-Djna.library.path=/opt/homebrew/lib"])
