(ns webjure.json-schema
  "Conform JSON schema to clojure.spec definitions"
  (:require [clojure.spec :as s]
            [cheshire.core :as cheshire]))


(defn from-json
  "Convert JSON schema validation keys from strings to 
  namespaced keywords in the webjure.json-schema namespace 
  without touching property names."
  ([json-schema] (schemaize json-schema true))
  ([json-schema convert-keys?]
   (reduce-kv
    (fn [m k v]
      (assoc m
             (if convert-keys?
               (keyword "webjure.json-schema" k)
               k)
             (if (map? v)
               (schemaize v (not= "properties" k))
               v)))
    {}
    json-schema)))

;; Helper predicates
(def boolean? #{true false})
(def non-negative-integer?
  (s/and integer? #(>= % 0)))

(s/def ::schema (s/keys))

(s/def ::type #{"array" "boolean" "integer" "number" "null" "object" "string"})
(s/def ::$ref string?)

;; Numeric 
(s/def ::minimum number?)
(s/def ::maximum number?)
(s/def ::multipleOf number?)
(s/def ::exclusiveMinimum boolean?)
(s/def ::exclusiveMaximum boolean?)

;; Strings
(s/def ::maxLength non-negative-integer?)
(s/def ::minLength non-negative-integer?)
(s/def ::pattern string?)

;; Arrays
(s/def ::items (s/or :schema ::schema
                     :schemas (s/* ::schema)))
(s/def ::additionalItems (s/or :boolean boolean?
                               :object ::schema))
(s/def ::maxItems non-negative-integer?)
(s/def ::minItems non-negative-integer?)

(s/def ::uniqueItems boolean?)

;; Objects
(s/def ::properties (s/map-of string? ::schema))
(s/def ::maxProperties non-negative-integer?)
(s/def ::minProperties non-negative-integer?)

(s/def ::required (s/and (s/* string?)
                         #(= (count %) (count (distinct %)))))
(s/def ::additionalProperties (s/or :allowed? boolean?
                                    :schema ::schema))
(s/def ::patternProperties (s/map-of string? ::schema))

(s/conform ::schema
           {::properties {"foo" {::type "integer"
                                 ::exclusiveMinimum true
                                 ::minimum 10}
                          "bars" {::items [{::type "string"}]
                                  ::additionalItems {::type "boolean"}}}})

;; (cheshire/parse-string "{\"properties\": {\"foo\": {\"type\": \"integer\", \"exclusiveMinimum\": 1, \"minimum\": 50}}}" #(keyword "webjure.json-schema" %))
