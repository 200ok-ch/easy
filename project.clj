(defproject easy "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [clj-time "0.15.2"]
                 [org.clojure/data.csv "1.1.0"]
                 [clj-commons/clj-yaml "1.0.27"]
                 [hbs "1.0.3"]
                 [org.clojure/tools.cli "1.1.230"]]
  :main ^:skip-aot easy.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:plugins [[lein-binplus "0.6.8"]]}}
  :bin {:name "easy"
        :bin-path "~/bin"
        :jvm-opts ["-server" "-Dfile.encoding=utf-8" "$JVM_OPTS" ]})
