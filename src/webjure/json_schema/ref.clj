(ns webjure.json-schema.ref
  (:require [cheshire.core :as cheshire]
            [clojure.java.io :as io]))

(defn resolve-ref [uri]
  (let [schema (io/file uri)]
    (when (.canRead schema)
      (cheshire/parse-string (slurp schema)))))

(defn resolve-schema [schema options]
  (if-let [ref (get schema "$ref")]
    (let [resolver (or (:ref-resolver options)
                       resolve-ref)
          referenced-schema (resolver ref)]
      (if-not referenced-schema
        ;; Throw exception if schema can't be loaded
        (throw (IllegalArgumentException.
                (str "Unable to resolve referenced schema: " ref)))
        referenced-schema))
    schema))
