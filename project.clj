(defproject webjure/json-schema "0.4"
  :description "Minimalistic JSON schema validator with $ref support."
  :url "https://github.com/tatut/json-schema"
  :license {:name "MIT License"
            :url  "http://www.opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [cheshire "5.5.0"]
                 [clj-time "0.10.0"]]
  :source-paths ["src"]
  :test-paths ["test"])
