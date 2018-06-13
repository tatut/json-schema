(ns webjure.json-schema.validator.macro
  "Macro version of validator. Loads and parses schema at compile time and
  emits code to check data for validity."
  (:require [webjure.json-schema.ref :refer [resolve-schema resolve-ref]]
            [webjure.json-schema.validator.string :as string]
            [webjure.json-schema.validator.format :as format]
            [clojure.string :as str]
            [webjure.json-schema.ref :as ref]))


(declare validate)

;; Below are the functions that emit validations.
;; They all have the same signature: they take a schema,
;; the symbol for the data, error and ok expansion functions
;; and the options map. They return nil if there is no check
;; to be done for this type or an expansion for the check.

(defn validate-type
  "Check one of the seven JSON schema core types"
  [{type "type"} data-sym error ok _]

  (when type
    (let [expand-type #(case %
                         "array" `(sequential? ~data-sym)
                         "boolean" `(instance? Boolean ~data-sym)
                         "integer" `(or (instance? Integer ~data-sym)
                                        (instance? Long ~data-sym)
                                        (instance? java.math.BigInteger ~data-sym))
                         "number" `(number? ~data-sym)
                         "null" `(nil? ~data-sym)
                         "object" `(map? ~data-sym)
                         "string" `(string? ~data-sym)
                         nil nil)
          ]
      (let [e (gensym "ERROR")]
        `(if ~(if (sequential? type)
                `(or ~@(map expand-type type))
                (expand-type type))
           ~(ok)
           (let [~e {:error :wrong-type
                     :expected ~(if (sequential? type)
                                 (into #{}
                                       (map keyword)
                                       type)
                                 (keyword type))
                     :data ~data-sym}]
             ~(error e)))))))

(defn type-is?
  "Check if a type validation is taking place. This can be used to
  elide instance checks in later validations."
  [{type "type"} & types]
  (let [types (into #{} types)]
    (and type
         (not (sequential? type)) ;; either type, can't be sure
         (types type))))

(defn validate-number-bounds [{min           "minimum" max "maximum"
                               exclusive-min "exclusiveMinimum"
                               exclusive-max "exclusiveMaximum"
                               multiple-of   "multipleOf" :as schema}
                              data error ok _]
  (when (or min max multiple-of)
    (let [e (gensym "E")]
      `(cond
         ;; If no type check has been done, add type check that
         ;; skips non-number types
         ~@(when-not (type-is? schema "integer" "number")
             [`(not (number? ~data))
              (ok)])

         ~@(when (and min exclusive-min)
             [`(<= ~data ~min)
              `(let [~e {:error     :out-of-bounds
                         :data      ~data
                         :minimum   ~min
                         :exclusive true}]
                 ~(error e))])

         ~@(when (and min (not exclusive-min))
             [`(< ~data ~min)
              `(let [~e {:error     :out-of-bounds
                         :data      ~data
                         :minimum   ~min
                         :exclusive false}]
                 ~(error e))])


         ~@(when (and max exclusive-max)
             [`(>= ~data ~max)
              `(let [~e {:error     :out-of-bounds
                         :data      ~data
                         :maximum   ~max
                         :exclusive true}]
                 ~(error e))])

         ~@(when (and max (not exclusive-max))
             [`(> ~data ~max)
              `(let [~e {:error     :out-of-bounds
                         :data      ~data
                         :maximum   ~max
                         :exclusive false}]
                 ~(error e))])

         ~@(when multiple-of
             [`(not (or (zero? ~data)
                        (let [d# (/ ~data ~multiple-of)]
                          (or (integer? d#)
                              (= (Math/floor d#) d#)))))
              `(let [~e {:error :not-multiple-of
                         :data ~data
                         :expected-multiple-of ~multiple-of}]
                 ~(error e))])

         :default
         ~(ok)))))

(defn validate-string-length [{min "minLength" max "maxLength" :as schema} data error ok _]
  (when (or min max)
    (let [e (gensym "E")]
      `(cond
         ~@(when-not (type-is? schema "string")
             [`(not (string? ~data))
              (ok)])

         ~@(when min
             [`(< (string/length ~data) ~min)
              `(let [~e {:error :string-too-short
                         :minimum-length ~min
                         :data ~data}]
                 ~(error e))])
         ~@(when max
             [`(> (string/length ~data) ~max)
              `(let [~e {:error :string-too-long
                         :maximum-length ~max
                         :data ~data}]
                 ~(error e))])

         :default
         ~(ok)))))

(defn validate-string-pattern [{pattern "pattern"} data error ok _]
  (when pattern
    (let [e (gensym "E")]
      `(if-not (string? ~data)
         ~(ok)
         (if (re-find ~(re-pattern pattern) ~data)
           ~(ok)
           (let [~e {:error :string-does-not-match-pattern
                     :pattern ~pattern
                     :data ~data}]
             ~(error e)))))))





(defn validate-string-format [{format "format" :as schema} data error ok
                              {lax-date-time-format? :lax-date-time-format?}]
  (when format
    (let [e (gensym "E")]
      `(if-let [~e (when ~data
                     (~(case format
                         "date-time" (if lax-date-time-format?
                                       'webjure.json-schema.validator.format/validate-lax-date-time
                                       'webjure.json-schema.validator.format/validate-date-time)
                         "hostname" 'webjure.json-schema.validator.format/validate-hostname
                         "ipv4" 'webjure.json-schema.validator.format/validate-ipv4
                         "ipv6" 'webjure.json-schema.validator.format/validate-ipv6
                         "uri" 'webjure.json-schema.validator.format/validate-uri
                         "email" 'webjure.json-schema.validator.format/validate-email
                         (do
                           (println "WARNING: Unsupported format: " format " in schema: " schema)
                           `(constantly nil)))
                      ~data))]
         ~(error e)
         ~(ok)))))

(defn validate-properties [{properties "properties"
                            pattern-properties "patternProperties"
                            additional-properties "additionalProperties"
                           :as        schema} data error ok options]
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
          property-names (into #{} (map first properties))
          e (gensym "E")
          property-errors (gensym "PROP-ERROR")
          v (gensym "V")
          d (gensym "DATA")
          props (gensym "PROPS")
          prop (gensym "PROP")
          extra-properties (gensym "EXTRA-PROPS")
          invalid-pp (gensym "INVALID-PP")]
      `(if-not (map? ~data)
         ~(ok)
         (let [~property-errors
               (as-> {} ~property-errors

                 ;; Check required props
                 ~@(for [p required]
                     `(if-not (contains? ~data ~p)
                        (assoc ~property-errors ~p {:error :missing-property})
                        ~property-errors))

                 ;; Property validations
                 ~@(for [[property-name property-schema] properties
                         :let [error (fn [error]
                                       `(assoc ~property-errors ~property-name ~error))
                               ok (constantly property-errors)]]
                     `(let [~v (get ~data ~property-name ::not-found)]
                        (if (= ::not-found ~v)
                          ;; not found for required fields is checked earlier
                          ~property-errors

                          ;; validate property by type
                          (if-let [~e ~(validate property-schema v options)]
                            ~(error e)
                            ~(ok)))))

                 ;; Validate pattern properties
                 ~@(for [[pattern schema] pattern-properties
                         :let [error (fn [error]
                                       `(assoc ~property-errors ~pattern ~error))
                               ok (constantly property-errors)]]
                     `(let [~invalid-pp (keep (fn [name#]
                                                (when (re-find ~(re-pattern pattern) name#)
                                                  (let [~v (get ~data name#)]
                                                    ~(validate schema v options))))
                                              (keys ~data))]
                        (if-not (empty? ~invalid-pp)
                          (let [~e {:error :invalid-pattern-properties
                                    :pattern ~pattern
                                    :schema ~schema
                                    :properties (into #{} ~invalid-pp)}]
                            ~(error e))
                          ~(ok))))
                 )]
           (if-not (empty? ~property-errors)
             (let [~e {:error      :properties
                       :data       ~data
                       :properties ~property-errors}]
               ~(error e))

             (let [~extra-properties ~(when-not (#{true {}} additional-properties)
                                        `(as-> (keys ~data) ~props
                                           (remove ~property-names ~props)
                                           ~@(when pattern-properties
                                               [`(remove
                                                  (fn [p#]
                                                    (some #(re-find % p#)
                                                          [~@(map re-pattern
                                                                  (keys pattern-properties))]))
                                                  ~props)])
                                           (into #{} ~props)))]
               ~(cond
                  ;; No additional properties allowed, signal error if there are any
                  (false? additional-properties)
                  `(if-not (empty? ~extra-properties)
                     ;; We have properties outside the schema, error
                     (let [~e {:error          :additional-properties
                               :property-names ~extra-properties}]
                       ~(error e))

                     ;; No errors
                     ~(ok))


                  ;; Additional properties is a schema, check all extra properties
                  ;; against schema
                  (and (map? additional-properties) (not= {} additional-properties))
                  `(let [invalid-additional-properties#
                         (into {}
                               (keep (fn [~prop]
                                          (let [~v (get ~data ~prop)
                                                e# ~(validate additional-properties v options)]
                                            (when e#
                                              [~prop e#]))))
                               ~extra-properties)]
                     (if-not (empty? invalid-additional-properties#)
                       (let [~e {:error :invalid-additional-properties
                                 :invalid-additional-properties invalid-additional-properties#
                                 :data ~data}]
                         ~(error e))
                       ~(ok)))

                  :default
                  (ok)))))))))

(defn validate-property-count [{min "minProperties" max "maxProperties" :as schema} data error ok _]
  (when (or min max)
    (let [e (gensym "E")]
      `(cond
         ~@(when-not (type-is? schema "object")
             [`(not (map? ~data))
              (ok)])

         ~@(when min
             [`(< (count ~data) ~min)
              `(let [~e {:error :too-few-properties
                         :minimum-properties ~min
                         :data ~data}]
                 ~(error e))])
         ~@(when max
             [`(> (count ~data) ~max)
              `(let [~e {:error :too-many-properties
                         :maximum-properties ~max
                         :data ~data}]
                 ~(error e))])

         :default
         ~(ok)))))

(defn validate-enum-value
  [{enum "enum"} data error ok _]
  (when-let [allowed-values (and enum (into #{} enum))]
    (let [e (gensym "E")]
      `(if-not (~allowed-values ~data)
         (let [~e {:error          :invalid-enum-value
                   :data           ~data
                   :allowed-values ~allowed-values}]
           ~(error e))
         ~(ok)))))

(defn validate-array-items [{item-schema "items"
                             additional-items "additionalItems" :as schema} data error ok options]
  (when item-schema
    (let [e (gensym "E")
          c (gensym "C")
          item (gensym "ITEM")
          item-error (gensym "ITEM-ERROR")]
      `(if-not (sequential? ~data)
         ~(ok)
         ~(cond
            ;; Schema is a map: validate all items against it
            (map? item-schema)
            `(loop [errors# []
                    i# 0
                    [~item & items#] ~data]
               (if-not ~item
                 (if (empty? errors#)
                   ~(ok)
                   (let [~e {:error :array-items
                             :data  ~data
                             :items errors#}]
                     ~(error e)))
                 (let [item-error#
                       ~(if (and (map? item-schema) (item-schema "enum"))
                          (validate-enum-value item-schema item
                                               identity
                                               (constantly nil)
                                               options)
                          (validate item-schema item options))]
                   (recur (if item-error#
                            (conj errors# (assoc item-error#
                                                 :position i#))
                            errors#)
                          (inc i#)
                          items#))))

            ;; Schema is an array, validate each index with its own schema
            (sequential? item-schema)
            `(let [~c (count ~data)]
               (if-let [~e (or
                            #_(when (< (count ~data) ~(count item-schema))
                                {:error :too-few-items
                                 :expected-item-count ~(count item-schema)
                                 :actual-item-count (count ~data)
                                 :data ~data})

                            ~@(for [i (range (count item-schema))]
                                `(when (> ~c ~i)
                                   (let [~item (nth ~data ~i)]
                                     ~(validate (nth item-schema i) item options))))

                            ;; If additional items is false, don't allow more items
                            ~@(when (false? additional-items)
                                [`(when (> ~c ~(count item-schema))
                                    {:error :additional-items-not-allowed
                                     :additional-items (drop ~(count item-schema) ~data)
                                     :data ~data})])

                            ;; If additional items is a schema, check each against it
                            ~@(when (map? additional-items)
                                [`(some (fn [~item]
                                          (when-let [e# ~(validate additional-items item options)]
                                            {:error :additional-item-does-not-match-schema
                                             :item-error e#
                                             :data ~data}))
                                        (drop ~(count item-schema) ~data))])
                            )]
                 ~(error e)
                 ~(ok))))))))

(defn validate-array-item-count [{min-items "minItems" max-items "maxItems"} data error ok options]
  (when (or min-items max-items)
    (let [e (gensym "E")
          items (gensym "ITEMS")]
      `(if-not (sequential? ~data)
         ~(ok)
         (let [~items (count ~data)]
           (cond
             ~@(when min-items
                 [`(> ~min-items ~items)
                  `(let [~e {:error :wrong-number-of-elements
                             :minimum ~min-items :actual ~items}]
                     ~(error e))])

             ~@(when max-items
                 [`(< ~max-items ~items)
                  `(let [~e {:error :wrong-number-of-elements
                             :maximum ~max-items :actual ~items}]
                     ~(error e))])

             :default
             ~(ok)))))))

(defn validate-array-unique-items [{unique-items "uniqueItems"} data error ok _]
  (when unique-items
    (let [item-count (gensym "IC")
          e (gensym "E")
          c (gensym "C")]
      `(if-not (sequential? ~data)
         ~(ok)
         (let [~c (count ~data)]
           (loop [seen# #{}
                  duplicates# #{}
                  i# 0]
             (if (= i# ~c)
               (if-not (empty? duplicates#)
                 (let [~e {:error :duplicate-items-not-allowed
                           :duplicates duplicates#}]
                   ~(error e))
                 ~(ok))
               (let [item# (nth ~data i#)]
                 (recur (conj seen# item#)
                        (if (seen# item#)
                          (conj duplicates# item#)
                          duplicates#)
                        (inc i#))))))))))

(defn validate-not [{schema "not"} data error ok options]
  (when schema
    (let [e (gensym "E")]
      `(let [~e ~(validate schema data options)]
         (if (nil? ~e)
           (let [~e {:error :should-not-match
                     :schema ~schema
                     :data ~data}]
             ~(error e))
           ~(ok))))))

(defn validate-all-of
  "Validate match against all of the given schemas."
  [{all-of "allOf"} data error ok options]
  (when-not (empty? all-of)
    (let [e (gensym "E")]
      `(if-not (and
                ~@(for [schema all-of]
                    `(nil? ~(validate schema data options))))
         (let [~e {:error :does-not-match-all-of
                   :all-of ~all-of
                   :data ~data}]
           ~(error e))
         ~(ok)))))

(defn validate-any-of
  "Validate match against any of the given schemas."
  [{any-of "anyOf"} data error ok options]
  (when-not (empty? any-of)
    (let [e (gensym "E")]
      `(let [errors# [~@(for [schema any-of]
                          (validate schema data options))]]
         (if (some nil? errors#)
           ~(ok)
           (let [~e {:error :does-not-match-any-of
                     :any-of ~any-of
                     :data ~data
                     :errors errors#}]
             ~(error e)))))))

(defn validate-one-of
  "Validate match against one (and only one) of the given schemas."
  [{one-of "oneOf"} data error ok options]
  (when-not (empty? one-of)
    (let [e (gensym "E")
          c (gensym "C")]
      `(let [~c (+ ~@(for [s one-of]
                       (validate s data (constantly 0) (constantly 1) options)))]
         (if-not (= 1 ~c)
           (let [~e {:error :item-does-not-match-exactly-one-schema
                     :data ~data
                     :matched-schemas ~c}]
             ~(error e))
           ~(ok))))))

(defn validate-dependencies [{dependencies "dependencies" :as schema} data error ok options]
  (when-not (empty? dependencies)
    (let [e (gensym "E" )]
      `(cond
         ~@(when-not (type-is? schema "object")
             [`(not (map? ~data))
              (ok)])

         ~@(mapcat
            (fn [[property schema-or-properties]]
              [`(and (contains? ~data ~property)
                     ~(if (map? schema-or-properties)
                        (validate schema-or-properties data options)
                        `(or ~@(for [p schema-or-properties]
                                 `(not (contains? ~data ~p))))))
               `(let [~e {:error :dependency-mismatch
                          :dependency {~property ~schema-or-properties}
                          :data ~data}]
                  ~(error e))])
            dependencies)

         :default
         ~(ok)))))

(def validations [#'validate-not #'validate-all-of #'validate-any-of #'validate-one-of
                  #'validate-dependencies
                  #'validate-type
                  #'validate-enum-value
                  #'validate-number-bounds
                  #'validate-string-length #'validate-string-pattern validate-string-format
                  #'validate-properties #'validate-property-count
                  #'validate-array-items #'validate-array-item-count #'validate-array-unique-items])

(defn validate
  ([schema data options]
   (validate schema data identity (constantly nil) options))
  ([schema data error ok options]
   (let [options (ref/root-schema options schema)
         schema (resolve-schema schema options)
         e (gensym "E")
         definitions (get schema "definitions")]
     `(or ~@(for [validate-fn validations
                  :let [form (validate-fn schema data error ok options)]
                  :when form]
              form)))))

(defmacro make-validator
  "Create a validator function. The schema and options will be evaluated at compile time.

  An map of options can be given that supports the keys:
  :ref-resolver    Function for loading referenced schemas. Takes in
                   the schema URI and must return the schema parsed form.
                   Default just tries to read it as a file via slurp and parse.

  :draft3-required  when set to true, support draft3 style required (in property definition),
                    defaults to false

  :lax-date-time-format?  when set to true, allow more variation in date format,
                          normally only strict RFC3339 dates are valid"
  ([schema options]
   (let [schema (eval schema)
         options (-> (eval options)
                     (assoc :ref-resolver (or (:ref-resolver options)
                                              (memoize ref/resolve-ref))))
         data (gensym "DATA")]
     `(fn [~data]
        ~(validate schema data options)))))
