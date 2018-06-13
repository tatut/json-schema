(defproject webjure/json-schema "20180613-SNAPSHOT"
  :description "JSON schema validator"
  :url "https://github.com/tatut/json-schema"
  :license {:name "MIT License"
            :url  "http://www.opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cheshire "5.6.1"]
                 [clj-time "0.11.0"]]
  :source-paths ["src"]
  :test-paths ["test"])
