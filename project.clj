(defproject gini "0.1.0-SNAPSHOT"
  :description "Gini Coefficient and Lorenz Curve"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [speclj "2.5.0"]

                 ;; 2.03
                 [clj-diff "1.0.0-SNAPSHOT"]

                 ;; 2.06
                 [clj-time "0.4.4"]

                 ;; 2.07
                 [org.clojure/data.csv "0.1.2"]

                 ;; 2.11
                 [parse-ez "0.3.4"]

                 ;; 2.12
                 ; [org.clojure/data.xml "0.0.6"]
                 [valip "0.2.0"]

                 [incanter/incanter-core "1.5.0-SNAPSHOT"]
                 [incanter/incanter-io "1.5.0-SNAPSHOT"]
                 [incanter/incanter-charts "1.5.0-SNAPSHOT"]
                 [incanter/incanter-mongodb "1.5.0-SNAPSHOT"]
                 [incanter/incanter-pdf "1.5.0-SNAPSHOT"]
                 [incanter/incanter-latex "1.5.0-SNAPSHOT"]
                 [incanter/incanter-excel "1.5.0-SNAPSHOT"]
                 [incanter/incanter-sql "1.5.0-SNAPSHOT"]
                 [incanter/incanter-zoo "1.5.0-SNAPSHOT"]
                 [swingrepl "1.3.0"
                  :exclusions [org.clojure/clojure
                               org.clojure/clojure-contrib]]
                 [jline "0.9.94"]

                 ;; https://github.com/thebusby/iota
                 [iota "1.0.3"]

                 ;; https://github.com/tebeka/clj-digest
                 [digest "1.3.0"]


                 ]
  :plugins [[speclj "2.1.2"]]
  :jvm-opts ["-Xmx768M"]
  ;:test-paths ["spec/"]
  :repl-options {:port 4555})
