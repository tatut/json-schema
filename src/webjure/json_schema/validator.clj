(ns webjure.json-schema.validator
  "Validator for JSON schema for draft 4"
  (:require [cheshire.core :as cheshire]
            [clojure.java.io :as io]))

(declare validate)

(defn resolve-ref [uri]
  (let [schema (io/file uri)]
    (when (.canRead schema)
      (cheshire/parse-string (slurp schema)))))
       
(defmulti validate-by-type
  "Validate JSON schema item by type. Returns a sequence of errors."
  (fn [schema data options] (get schema "type")))

(defmethod validate-by-type "object"
  [{properties "properties"
    :as schema} data options]
  
  (if-not (map? data)
    [{:error :wrong-type :expected :map
      :data data}]

    (let [required? (into #{}
                          (get schema "required"))
          errors (into
                  {}
                  (keep identity 
                        (for [[property-name property-schema] properties
                              :let [property-value (get data property-name)]]
                          (if (and (nil? property-value)
                                   (required? property-name))
                            [property-name {:error :missing-property}]
                            (when property-value
                              (when-let [error (validate property-schema property-value options)]
                                [property-name error]))))))]
      (if-not (empty? errors)
        {:error :properties
         :data data
         :properties errors}

        (let [property-names (into #{} (map first properties))
              extra-properties (into #{}
                                     (keep #(when-not (property-names %) %)
                                           (keys data)))]
          (if-not (empty? extra-properties)
            ;; We have properties outside the schema, error
            ;; FIXME: check additionalProperties flag
            {:error :additional-properties
             :property-names extra-properties}

            ;; No errors
            nil))))))


(defmethod validate-by-type "integer"
  [_ data _]
  (when-not (integer? data)
    {:error :wrong-type :expected :integer :data data}))

(defmethod validate-by-type "string"
  [{enum "enum"} data _]
  (if-not (string? data)
    {:error :wrong-type :expected :string :data data}
    (when enum
      (let [allowed-values (into #{} enum)]
        (when-not (allowed-values data)
          {:error :invalid-enum-value
           :data data
           :allowed-values allowed-values})))))

(defmethod validate-by-type "array"
  [{item-schema "items"} data options]
  (if-not (sequential? data)
    {:error :wrong-type :expected :array-like :data data}
    (loop [errors []
           i 0
           [item & items] data]
      (if-not item
        (if (empty? errors)
          nil
          {:error :array-items
           :data data
           :items errors})
        (let [item-error (validate-by-type item-schema item options)]
          (recur (if item-error
                   (conj errors (assoc item-error
                                  :position i))
                   errors)
                 (inc i)
                 items))))))

(defn validate
  "Validate the given data with the given schema. Both must be in Clojure format with string keys.
  Returns nil on an error description map.

  An map of options can be given that supports the keys:
  :ref-resolver    Function for loading referenced schemas. Takes in 
                   the schema URI and must return the schema parsed form.
                   Default just tries to read it as a file via slurp and parse."
  ([schema data] (validate schema data {:ref-resolver resolve-ref}))
  ([schema data options]
   (if-let [ref (get schema "$ref")]
     (let [referenced-schema ((:ref-resolver options) ref)]
       (if-not referenced-schema
         {:error :unable-to-resolve-referenced-schema
          :schema-uri ref}
         (validate referenced-schema data options)))
     (validate-by-type schema data options))))
  
  

