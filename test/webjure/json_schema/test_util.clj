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
        validation-opts (or (first opts) {})
        test-opts (merge {:macro true :fn true}
                         (second opts))
        data (gensym "DATA")
        macro-validator (gensym "MACRO-VALIDATOR")
        fn-res (gensym "FN-RES")
        macro-res (gensym "MACRO-RES")]
    ;;(println "EXPAND TEST VALIDATOR FOR " (pr-str schema) " TEST-OPTS: " (pr-str test-opts))
    `(let [~macro-validator ~(when (:macro test-opts)
                               `(make-validator ~schema ~validation-opts))]
       (fn [~data]
         (let [~fn-res ~(when (:fn test-opts)
                          `(validate ~schema ~data ~validation-opts))
               ~macro-res ~(when (:macro test-opts)
                             `(~macro-validator ~data))]
           ~(when (and (:macro test-opts) (:fn test-opts))
              `(is (= ~fn-res ~macro-res)
                   "function and macro versions validate in the same way"))
           ~(cond
              (:fn test-opts)
              fn-res

              (:macro test-opts)
              macro-res))))))

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
