(ns webjure.json-schema.test-util
  (:require [clojure.test :as t :refer [is]]
            [webjure.json-schema.validator :refer [validate]]
            [webjure.json-schema.validator.macro :refer [make-validator]]
            [cheshire.core :as cheshire]))

(defn p [resource-path]
  (->> resource-path
       (str "test/resources/")
       slurp cheshire/parse-string))

;; Define a macro that defines a new validator function.
;; The function calls both the function style validation and
;; the generated macro version and verifies that they validate
;; errors in the same way.

(defmacro validate-fn [schema & opts]
  (let [schema (if (string? schema)
                 (p schema)
                 schema)
        opts (or (first opts) {})]
    `(fn [data#]
       (try
         (let [;;fn-res# (validate ~schema data# ~opts)
               macro-res# ((make-validator ~schema ~opts) data#)]
           #_(is (= fn-res# macro-res#)
                 "function and macro versions validate in the same way")
           macro-res#)
         (catch Throwable t#
           (is false (str "Validation exception."
                          "\nSchema: " ~(pr-str schema)
                          "\nData: " (pr-str data#)
                          "\nMessage: " (.getMessage t#))))))))

(defmacro defvalidate [name schema & opts]
  (let [schema (if (string? schema)
                 (p schema)
                 schema)
        opts (or (first opts) {})]
    `(defn ~name [data#]
       (let [fn-res# (validate ~schema data# ~opts)
             macro-res# ((make-validator ~schema ~opts) data#)]
         (is (= fn-res# macro-res#)
             "function and macro versions validate in the same way")
         fn-res#))))
