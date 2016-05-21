(ns webjure.json-schema.validator
  "Validator for JSON schema for draft 4"
  (:require [clojure.string :as str]
            [webjure.json-schema.ref :as ref :refer [resolve-ref resolve-schema]]
            [webjure.json-schema.validator
             [format :as format]
             [string :as string]]))

(declare validate)

(defn validate-type
  "Check one of the seven JSON schema core types"
  [{type "type"} data _]

  (when type
    (let [check-type #(case %
                        "array" (sequential? data)
                        "boolean" (instance? Boolean data)
                        "integer" (or (instance? Integer data)
                                      (instance? Long data)
                                      (instance? java.math.BigInteger data))
                        "number" (number? data)
                        "null" (nil? data)
                        "object" (map? data)
                        "string" (string? data)
                        nil nil)]

      (when-not (if (sequential? type)
                  (some check-type type)
                  (check-type type))
        {:error :wrong-type
         :expected (if (sequential? type)
                     (into #{}
                           (map keyword)
                           type)
                     (keyword type))
         :data data}))))


(defn validate-number-bounds [{min           "minimum" max "maximum"
                               exclusive-min "exclusiveMinimum"
                               exclusive-max "exclusiveMaximum"
                               multiple-of   "multipleOf" :as schema}
                              data _]
  (when (or min max multiple-of)
    (cond
      (not (number? data))
      nil

      (and min exclusive-min
           (<= data min))
      {:error     :out-of-bounds
       :data      data
       :minimum   min
       :exclusive true}

      (and min (not exclusive-min)
           (< data min))
      {:error     :out-of-bounds
       :data      data
       :minimum   min
       :exclusive false}


      (and max exclusive-max
           (>= data max))
      {:error     :out-of-bounds
       :data      data
       :maximum   max
       :exclusive true}

      (and max (not exclusive-max)
           (> data max))
      {:error     :out-of-bounds
       :data      data
       :maximum   max
       :exclusive false}


      (and multiple-of
           (not (or (zero? data)
                    (let [d (/ data multiple-of)]
                      (or (integer? d)
                          (= (Math/floor d) d))))))
      {:error :not-multiple-of
       :data data
       :expected-multiple-of multiple-of}

      :default
      nil)))

(defn validate-string-length [{min "minLength" max "maxLength" :as schema} data _]
  (when (or min max)
    (cond
      (not (string? data))
      nil

      (and min
           (< (string/length data) min))
      {:error :string-too-short
       :minimum-length min
       :data data}

      (and max
           (> (string/length data) max))
      {:error :string-too-long
       :maximum-length max
       :data data}

      :default
      nil)))

(defn validate-string-pattern [{pattern "pattern"} data _]
  (when pattern
    (if (or (not (string? data))
            (re-find (re-pattern pattern) data))
      nil
      {:error :string-does-not-match-pattern
       :pattern pattern
       :data data})))

(defn validate-string-format [{format "format"} data
                              {lax-date-time-format? :lax-date-time-format?}]
  (when format
    ((case format
       "date-time" (if lax-date-time-format?
                     format/validate-lax-date-time
                     format/validate-date-time)
       "hostname" format/validate-hostname
       "ipv4" format/validate-ipv4
       "ipv6" format/validate-ipv6
       "uri" format/validate-uri
       "email" format/validate-email
       (do
         (println "WARNING: Unsupported format: " format)
         (constantly nil)))
     data)))

(defn validate-properties [{properties "properties"
                            pattern-properties "patternProperties"
                            additional-properties "additionalProperties"
                           :as        schema} data options]

  (when (or properties additional-properties pattern-properties)
    (let [properties (or properties {})
          additional-properties (if (nil? additional-properties) {} additional-properties)
          required (if (:draft3-required options)
                     ;; Draft 3 required is an attribute of the property schema
                     (into #{}
                           (for [[property-name property-schema] properties
                                 :when (get property-schema "required")]
                             property-name))

                     ;; Draft 4 has separate required attribute with a list of property names
                     (into #{}
                           (get schema "required")))
          property-names (into #{} (map first properties))]
      (if-not (map? data)
        nil
        (let [property-errors
              (as-> {} property-errors

                ;; Check required props
                (merge property-errors
                       (into {}
                             (keep (fn [p]
                                     (when-not (contains? data p)
                                       [p {:error :missing-property}])))
                             required))

                ;; Property validations
                (merge property-errors
                       (into {}
                             (keep (fn [[property-name property-schema]]
                                     (let [v (get data property-name)]
                                       (when-not (nil? v)
                                         ;; nil values for required fields are checked earlier
                                         (when-let [e (validate property-schema v options)]
                                           [property-name e])))))
                             properties))

                ;; Validate pattern properties
                (merge property-errors
                       (into {}
                             (keep
                              (fn [[pattern schema]]
                                (let [invalid-pp
                                      (keep (fn [name]
                                              (when (re-find (re-pattern pattern) name)
                                                (let [v (get data name)]
                                                  (validate schema v options))))
                                            (keys data))]
                                  (when-not (empty? invalid-pp)
                                    [pattern {:error :invalid-pattern-properties
                                              :pattern pattern
                                              :schema schema
                                              :properties (into #{} invalid-pp)}]))))
                             pattern-properties)))]
          (if-not (empty? property-errors)
            {:error      :properties
             :data       data
             :properties property-errors}

            (let [extra-properties (when-not (#{true {}} additional-properties)
                                     (as-> (keys data) props
                                       (remove property-names props)
                                       (if pattern-properties
                                         (remove
                                          (fn [p]
                                            (some #(re-find % p)
                                                  (map re-pattern
                                                       (keys pattern-properties))))
                                          props)
                                         props)
                                       (into #{} props)))]
              (cond
                ;; No additional properties allowed, signal error if there are any
                (false? additional-properties)
                (if-not (empty? extra-properties)
                  ;; We have properties outside the schema, error
                  {:error          :additional-properties
                   :property-names extra-properties}

                  ;; No errors
                  nil)


                ;; Additional properties is a schema, check all extra properties
                ;; against schema
                (and (map? additional-properties) (not= {} additional-properties))
                (let [invalid-additional-properties
                      (into {}
                            (keep (fn [prop]
                                    (let [v (get data prop)
                                          e (validate additional-properties v options)]
                                      (when e
                                        [prop e]))))
                            extra-properties)]
                  (when-not (empty? invalid-additional-properties)
                    {:error :invalid-additional-properties
                     :invalid-additional-properties invalid-additional-properties
                     :data data}))

                :default
                nil))))))))

(defn validate-property-count [{min "minProperties" max "maxProperties" :as schema} data _]
  (when (or min max)
    (cond
      (not (map? data))
      nil

      (and min (< (count data) min))
      {:error :too-few-properties
       :minimum-properties min
       :data data}

      (and max (> (count data) max))
      {:error :too-many-properties
       :maximum-properties max
       :data data}

      :default
      nil)))

(defn validate-enum-value
  [{enum "enum"} data _]
  (when-let [allowed-values (and enum (into #{} enum))]
    (when-not (allowed-values data)
      {:error          :invalid-enum-value
       :data           data
       :allowed-values allowed-values})))

(defn validate-array-items [{item-schema "items"
                             additional-items "additionalItems" :as schema} data options]
  (when item-schema
    (let [c (count data)]
      (if-not (sequential? data)
        nil


        (cond
          ;; Schema is a map: validate all items against it
          (map? item-schema)
          (loop [errors []
                 i 0]
            (if (= i c)
              (when-not (empty? errors)
                {:error :array-items
                 :data  data
                 :items errors})
              (let [item (nth data i)
                    item-error
                    (if (and (map? item-schema) (item-schema "enum"))
                      (validate-enum-value item-schema item options)
                      (validate item-schema item options))]
                (recur (if item-error
                         (conj errors (assoc item-error
                                             :position i))
                         errors)
                       (inc i)))))

          ;; Schema is an array, validate each index with its own schema
          (sequential? item-schema)
          (or (and (< c (count item-schema))
                   {:error :too-few-items
                    :expected-item-count (count item-schema)
                    :actual-item-count c
                    :data data})

              (first
               (for [i (range (count item-schema))
                     :let [item (nth data i)
                           e (validate (nth item-schema i) item options)]
                     :when e]
                 e))

              ;; If additional items is false, don't allow more items
              (and (false? additional-items)
                   (> (count data) (count item-schema))
                   {:error :additional-items-not-allowed
                    :additional-items (drop (count item-schema) data)
                    :data data})

              ;; If additional items is a schema, check each against it
              (and (map? additional-items)
                   (some (fn [item]
                           (when-let [e (validate additional-items item options)]
                             {:error :additional-item-does-not-match-schema
                              :item-error e
                              :data data}))
                         (drop (count item-schema) data)))))))))

(defn validate-array-item-count [{min-items "minItems" max-items "maxItems"} data options]
  (when (or min-items max-items)
    (if-not (sequential? data)
      nil
      (let [items (count data)]
        (cond
          (and min-items (> min-items items))
          {:error :wrong-number-of-elements
           :minimum min-items :actual items}

          (and max-items (< max-items items))
          {:error :wrong-number-of-elements
           :maximum max-items :actual items}

          :default
          nil)))))

(defn validate-array-unique-items [{unique-items "uniqueItems"} data _]
  (when unique-items
    (if-not (sequential? data)
      nil
      (let [c (count data)]
        (loop [seen #{}
               duplicates #{}
               i 0]
          (if (= i c)
            (when-not (empty? duplicates)
              {:error :duplicate-items-not-allowed
               :duplicates duplicates})
            (let [item (nth data i)]
              (recur (conj seen item)
                     (if (seen item)
                       (conj duplicates item)
                       duplicates)
                     (inc i)))))))))

(defn validate-not [{schema "not"} data options]
  (when schema
    (let [e (validate schema data options)]
      (when (nil? e)
        {:error :should-not-match
         :schema schema
         :data data}))))

(defn validate-all-of
  "Validate match against all of the given schemas."
  [{all-of "allOf"} data options]
  (when-not (empty? all-of)
    (when-not (every? #(nil? (validate % data options)) all-of)
      {:error :does-not-match-all-of
       :all-of all-of
       :data data})))

(defn validate-any-of
  "Validate match against any of the given schemas."
  [{any-of "anyOf"} data options]
  (when-not (empty? any-of)
    (when-not (some #(nil? (validate % data options)) any-of)
      {:error :does-not-match-any-of
       :any-of any-of
       :data data})))

(defn validate-one-of
  "Validate match against one (and only one) of the given schemas."
  [{one-of "oneOf"} data options]
  (when-not (empty? one-of)
    (let [c (reduce (fn [c schema]
                      (if (nil? (validate schema data options))
                        (inc c)
                        c))
                    0 one-of)]
      (when-not (= 1 c)
        {:error :item-does-not-match-exactly-one-schema
         :data data
         :matched-schemas c}))))

(defn validate-dependencies [{dependencies "dependencies" :as schema} data options]
  (when (and (not (empty? dependencies))
             (map? data))
    (some (fn [[property schema-or-properties]]
            (when (and (contains? data property)
                       (if (map? schema-or-properties)
                         (validate schema-or-properties data options)
                         (not (every? #(contains? data %) schema-or-properties))))
              {:error :dependency-mismatch
               :dependency {property schema-or-properties}
               :data data}))
          dependencies)))

(def validations [#'validate-not #'validate-all-of #'validate-any-of #'validate-one-of
                  #'validate-dependencies
                  #'validate-type
                  #'validate-enum-value
                  #'validate-number-bounds
                  #'validate-string-length #'validate-string-pattern validate-string-format
                  #'validate-properties #'validate-property-count
                  #'validate-array-items #'validate-array-item-count #'validate-array-unique-items])

(defn validate
  "Validate data against the given schema.

  An map of options can be given that supports the keys:
  :ref-resolver    Function for loading referenced schemas. Takes in
                   the schema URI and must return the schema parsed form.
                   Default just tries to read it as a file via slurp and parse.

  :draft3-required  when set to true, support draft3 style required (in property definition),
                    defaults to false

  :lax-date-time-format?  when set to true, allow more variation in date format,
                          normally only strict RFC3339 dates are valid"
  [schema data options]
  (let [options (-> options
                    (ref/root-schema schema)
                    (assoc :ref-resolver (or (:ref-resolver options)
                                             (memoize ref/resolve-ref))))
        schema (resolve-schema schema options)
        definitions (get schema "definitions")]
    (some #(% schema data options) validations)))
