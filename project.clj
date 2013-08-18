(defproject baseet-twdb "0.1.2-SNAPSHOT"
  :description "Simple library for accessing baseet tweets db"
  :url "https://github.com/bass3m/baseet-twdb"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev  {:plugins  [[lein-ancient "0.4.4"]]}}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.ashafa/clutch "0.4.0-RC1"]
                 [suweet "0.1.6-SNAPSHOT"]])
