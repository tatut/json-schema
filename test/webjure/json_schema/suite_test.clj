(ns webjure.json-schema.suite-test
  "Tests for https://github.com/json-schema-org/JSON-Schema-Test-Suite.
  The test suite is added as a submodule under test/resources."
  (:require  [clojure.test :as t :refer [deftest is testing]]
             [webjure.json-schema.test-util :refer [validate-fn]]
             [clojure.java.io :as io]
             [cheshire.core :as cheshire]
             [clojure.string :as str]))

(def excluded-files #{;; creating a validator fails for an invalid definition
                      ;; test this separately
                      "definitions.json"

                      ;; FIXME: create a resolver for these
                      ;; the normal refs mostly work, but large linked
                      ;; schema causes problems with the macros
                      "refRemote.json"
                      "ref.json"
                      })

(def suite-tests
  (->> (io/file "test/resources/JSON-Schema-Test-Suite/tests/draft4")
       file-seq
       (filter #(.endsWith (.getName %) ".json"))
       (filter #(not (excluded-files (.getName %))))
       (mapv (juxt #(let [n (.getName %)]
                      (subs n 0 (- (count n) 5)))
                   #(cheshire/parse-string (slurp %))))
       (into {})))

(defn sym [name]
  (symbol (str/replace name " " "-")))

(defmacro define-suite-tests []
  (let [schema-sym (gensym "SCHEMA")]
    `(do
       ~@(for [[test-name tests] suite-tests]
           `(deftest ~(sym (str "suite-" test-name))
              ~@(for [{desc "description"
                       schema "schema"
                       tests "tests"} tests]
                  `(testing ~desc
                     (let [~schema-sym (validate-fn ~schema {})]
                       ~@(for [{desc "description"
                                data "data"
                                valid? "valid"} tests]
                           (if valid?
                             `(is (nil? (~schema-sym ~data))
                                  ~(str "Test '" desc "' should be valid"))
                             `(is (~schema-sym ~data)
                                  ~(str "Test '" desc "' should NOT be valid"))))))))))))


(define-suite-tests)
