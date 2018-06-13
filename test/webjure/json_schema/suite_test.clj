(ns webjure.json-schema.suite-test
  "Tests for https://github.com/json-schema-org/JSON-Schema-Test-Suite.
  The test suite is added as a submodule under test/resources."
  (:require  [clojure.test :as t :refer [deftest is testing]]
             [webjure.json-schema.test-util :refer [validate-fn]]
             [clojure.java.io :as io]
             [cheshire.core :as cheshire]
             [clojure.string :as str]
             [webjure.json-schema.ref :as ref]))

(defn ref-resolver [uri]
  (ref/resolve-ref (str/replace uri
                                "http://localhost:1234"
                                "file:test/resources/JSON-Schema-Test-Suite/remotes/")))

(def exclusions
  {;; Skip invalid definition in the schema
   "definitions" {:skip true} ;; {"invalid definition" {:macro false :fn false}}

   ;; Skip recursive schema in the macro version as it
   ;; creates an ever expanding function (causing a stack overflow)
   "ref" {"root pointer ref" {:macro false}
          "remote ref, containing refs itself" {:macro false}
          "Recursive references between schemas" {:skip true}
          :options {:ref-resolver ref-resolver}}

   ;; We don't have a regex for a regex
   "ecmascript-regex" {:skip true}

   ;; Cheat (a little bit) here, change http URLs to file URIs so that
   ;; we don't need to start an HTTP server. Clojure slurp works for
   ;; both URI types the same.
   "refRemote" {"base URI change - change folder" {:skip true}
                "base URI change - change folder in subschema" {:skip true}
                "root ref in remote ref" {:skip true}
                :options {:ref-resolver ref-resolver}}

   "format" {"validation of date-time strings" {"a valid date-time string without second fraction" {:skip true}}}
   })

(def suite-tests
  (->> (io/file "test/resources/JSON-Schema-Test-Suite/tests/draft4")
       file-seq
       (filter #(.endsWith (.getName %) ".json"))
       (mapv (juxt #(let [n (.getName %)]
                      (subs n 0 (- (count n) 5)))
                   #(cheshire/parse-string (slurp %))))
       (into {})))

(defn sym [name]
  (symbol (str/replace name " " "-")))

(defmacro define-suite-tests []
  (let [schema-sym (gensym "SCHEMA")]
    `(do
       ~@(for [[test-name tests] suite-tests
               :let [options (or (get-in exclusions [test-name :options]) {})]
               :when (not (:skip (exclusions test-name)))]
           `(deftest ~(sym (str "suite-" test-name))
              ~@(for [{desc "description"
                       schema "schema"
                       tests "tests"} tests
                      :let [schema (ref/initialize-id-path schema)
                            options (merge options (get-in exclusions [test-name desc :options]))]
                      :when (not (:skip (get-in exclusions [test-name desc])))]
                  `(testing ~desc
                     (let [~schema-sym (validate-fn ~schema ~options ~(get-in exclusions [test-name desc]))]
                       ~@(for [{test-desc "description"
                                data "data"
                                valid? "valid"} tests
                               :when (not (:skip (get-in exclusions [test-name desc test-desc])))]
                           (if valid?
                             `(is (nil? (~schema-sym ~data))
                                  ~(str "Test '" test-desc "' should be valid"))
                             `(is (~schema-sym ~data)
                                  ~(str "Test '" test-desc "' should NOT be valid"))))))))))))

(define-suite-tests)
