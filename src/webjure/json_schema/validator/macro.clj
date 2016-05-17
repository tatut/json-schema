(ns webjure.json-schema.validator.macro
  "Macro version of validator. Loads and parses schema at compile time and
  emits code to check data for validity."
  (:require [clojure.java.io :as io]
            [cheshire.core :as cheshire]
            [clj-time.coerce :as time]))

(defn resolve-ref [uri]
  (let [schema (io/file uri)]
    (when (.canRead schema)
      (cheshire/parse-string (slurp schema)))))

(defmulti validate-by-type
  "Create expansion code for JSON schema validation by type. The expansion takes the data
  symbol, callbacks to create expansions for error and ok validations and the options map."
  (fn [schema data-sym error ok options] (get schema "type")))

(defn error [error-sym e]
  `(conj ~error-sym ~e))

(defn- resolve-schema [schema options]
  (if-let [ref (get schema "$ref")]
    (let [referenced-schema ((:ref-resolver options) ref)]
      (if-not referenced-schema
        ;; Throw compile time error if schema can't be loaded
        (throw (IllegalArgumentException.
                (str "Unable to resolve referenced schema: " ref)))
        referenced-schema))
    schema))

(declare validate-enum-value)
(defn validate [{enum :enum :as schema} data error ok options]
  (let [schema (resolve-schema schema options)
        e (gensym "E")]
    `(or ~(validate-enum-value schema data identity (constantly nil))
         ~(validate-by-type schema data error ok options))))

(defmethod validate-by-type "object"
  [{properties "properties"
    :as        schema} data
   error ok options]

  (let [required (if (:draft3-required options)
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
        d (gensym "DATA")]
    `(if-not (map? ~data)
       ~(error {:error :wrong-type :expected :map
                :data  data})
       (let [~property-errors
             (as-> {} ~property-errors

               ;; Check required props
               ~@(for [p required]
                   `(if (nil? (get ~data ~p))
                      (assoc ~property-errors ~p {:error :missing-property})
                      ~property-errors))

               ;; Property validations
               ~@(for [[property-name property-schema] properties
                       :let [error (fn [error]
                                     `(assoc ~property-errors ~property-name ~error))
                             ok (constantly property-errors)]]
                   `(let [~v (get ~data ~property-name)]
                      (if (nil? ~v)
                        ;; nil values for required fields are checked earlier
                        ~property-errors

                        ;; validate property by type
                        ~(validate property-schema v
                                   error ok options)))))]
         (if-not (empty? ~property-errors)
           (let [~e {:error      :properties
                     :data       ~data
                     :properties ~property-errors}]
             ~(error e))

           (let [property-names# ~property-names
                 extra-properties# (into #{}
                                         (keep #(when-not (property-names# %) %)
                                               (keys ~data)))]
             (if-not (empty? extra-properties#)
               ;; We have properties outside the schema, error
               ;; FIXME: check additionalProperties flag
               (let [~e {:error          :additional-properties
                         :property-names extra-properties#}]
                 ~(error e))

               ;; No errors
               ~(ok))))))))

(defn validate-number-bounds [{min           "minimum" max "maximum"
                               exclusive-min "exclusiveMinimum"
                               exclusive-max "exclusiveMaximum"
                               multiple-of   "multipleOf"}
                              data error ok]
  (let [e (gensym "E")]
    `(cond
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
           [`(not= 0 (rem ~data ~multiple-of))
            `(let [~e {:error :not-multiple-of
                       :data ~data
                       :expected-multiple-of ~multiple-of}]
               ~(error e))])

       :default
       ~(ok))))

(defmethod validate-by-type "integer"
  [schema data error ok _]
  (let [e (gensym "E")]
    `(if-not (integer? ~data)
       (let [~e {:error :wrong-type :expected :integer :data ~data}]
         ~(error e))
       ~(validate-number-bounds schema data error ok))))

(defmethod validate-by-type "number"
  [schema data error ok _]
  (let [e (gensym "E")]
    `(if-not (number? ~data)
       (let [~e {:error :wrong-type :expected :number :data ~data}]
         ~(error e))
       ~(validate-number-bounds schema data error ok))))

(defn validate-enum-value
  [{enum "enum"} data error ok]
  (let [e (gensym "E")]
    (if-let [allowed-values (when enum
                              (into #{} enum))]
      `(if-not (~allowed-values ~data)
         (let [~e {:error          :invalid-enum-value
                   :data           ~data
                   :allowed-values ~allowed-values}]
           ~(error e))
         ~(ok))
      (ok))))

(defmethod validate-by-type "string"
  [schema data error ok _]
  (let [e (gensym "E")]
    `(if-not (string? ~data)
       (let [~e {:error :wrong-type :expected :string :data ~data}]
         ~(error e))
       ~(if-let [format (get schema "format")]
          (cond
            (= format "date-time")
            `(if (nil? (time/from-string ~data))
               (let [~e {:error :wrong-format :expected :date-time :data ~data}]
                 ~(error e))
               ~(ok))

            :default
            (ok))
          (ok)))))

(defmethod validate-by-type "boolean"
  [_ data error ok _]
  (let [e (gensym "E")]
    `(if (nil? (#{true false} ~data))
       (let [~e  {:error :wrong-type :expected :boolean :data ~data}]
         ~(error e))
       ~(ok))))

(defn validate-array-items [options item-schema data error ok]
  (let [e (gensym "E")
        item (gensym "ITEM")
        item-error (gensym "ITEM-ERROR")]
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
                                       (constantly nil))
                  (validate item-schema item
                            identity
                            (constantly nil)
                            options))]
           (recur (if item-error#
                    (conj errors# (assoc item-error#
                                        :position i#))
                    errors#)
                  (inc i#)
                  items#))))))

(defmethod validate-by-type "array"
  [{min-items "minItems" max-items "maxItems"
    item-schema "items"
    unique-items "uniqueItems"} data error ok options]
  (let [e (gensym "E")
        items (gensym "ITEMS")]
    `(if-not (sequential? ~data)
       (let [~e {:error :wrong-type :expected :array-like :data ~data}]
         ~(error e))
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

           ~@(when unique-items
               [`(not= ~items (count (into #{} ~data)))
                `(let [~e {:error :duplicate-items-not-allowed}]
                   ~(error e))])

           :else ~(validate-array-items options item-schema data
                                        error ok))))))


(defmethod validate-by-type :default
  [schema data error ok _]
  ;; If no type is specified, anything goes
  (ok))

(defmacro make-validator
  "Create a validator function. The schema and options will be evaluated at compile time.

  An map of options can be given that supports the keys:
  :ref-resolver    Function for loading referenced schemas. Takes in
                   the schema URI and must return the schema parsed form.
                   Default just tries to read it as a file via slurp and parse.

  :draft3-required  when set to true, support draft3 style required (in property definition),
                    defaults to false"
  ([schema options]
   (let [schema (eval schema)
         options (merge {:ref-resolver resolve-ref}
                        (eval options))
         data (gensym "DATA")]
     `(fn [~data]
        ~(validate schema data
                   identity
                   (constantly nil)
                   options)))))
