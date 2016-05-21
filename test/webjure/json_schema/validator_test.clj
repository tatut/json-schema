(ns webjure.json-schema.validator-test
  (:require [cheshire.core :as cheshire]
            [clojure.test :refer [deftest is testing]]
            [webjure.json-schema.test-util :refer [defvalidate p]]))

(defvalidate address-and-phone "address-and-phone.schema.json")

(deftest validate-address-and-phone
  (testing "jsonschema.net example schema"
    (testing "valid json returns nil errors"
      (is (nil? (address-and-phone (p "address-and-phone-valid.json")))))

    (testing "missing property error is reported"
      (let [e (address-and-phone (p "address-and-phone-city-and-code-missing.json"))]
        (is (= :properties (:error e)))
        ;; "city" is reported as missing because it is required
        (is (= :missing-property (get-in e [:properties "address" :properties "city" :error])))

        ;; no errors in "phoneNumber" because missing "code" in first item is not required
        (is (nil? (get-in e [:properties "phoneNumber"])))))

    (testing "additional properties are ok"
      (is (nil? (address-and-phone (p "address-and-phone-additional-properties.json")))))))

(defvalidate ref-schema "person.schema.json")
(deftest validate-referenced-schema
  (testing "person schema that links to address and phone schema"
    (let [s (p "person.schema.json")]
      (testing "valid json returns nil errors"
        (is (nil? (ref-schema (p "person-valid.json")))))
      (testing "linked schema errors are reported"
        (is (= :missing-property
               (get-in (ref-schema (p "person-invalid.json"))
                       [:properties "contact" :properties "phoneNumber" :error])))))))

(defvalidate valid-enum {"type" "string" "enum" ["foo" "bar"]})
(deftest validate-enum
  (testing "enum values are checked"
    (is (nil? (valid-enum "foo")))
    (is (= {:error          :invalid-enum-value
            :data           "xuxu"
            :allowed-values #{"foo" "bar"}}
           (valid-enum "xuxu")))))


(defvalidate bool {"type" "boolean"})
(deftest validate-boolean
  (testing "boolean values are checked"
    (is (nil? (bool true)))
    (is (nil? (bool false)))
    (is (every? #(= :wrong-type (:error %))
                (map #(bool %)
                     ["foo" 42 [4 5] {"name" "Test"}])))))

(defvalidate validate-integer {"type" "integer" "minimum" 1 "maximum" 10})
(defvalidate validate-integer-excl {"type" "integer"
                                    "minimum" 1
                                    "maximum" 10
                                    "exclusiveMinimum" true
                                    "exclusiveMaximum" true})

(deftest validate-integer-bounds
  (testing "integer bounds are checked"
    (testing "non exclusive bounds work"
      (testing "valid values return nil"
        (is (every? nil?
                    (map #(validate-integer %)
                         (range 1 1)))))

      (testing "too low values report error with minimum"
        (is (= {:error :out-of-bounds :data 0 :minimum 1 :exclusive false}
               (validate-integer 0))))

      (testing "too high values report error with maximum"
        (is (= {:error :out-of-bounds :data 11 :maximum 10 :exclusive false}
               (validate-integer 11)))))
    (testing "exclusive bounds works"
      (is (nil? (validate-integer-excl 2)))
      (is (nil? (validate-integer-excl 9)))
      (is (= {:error :out-of-bounds :data 1 :minimum 1 :exclusive true}
             (validate-integer-excl 1)))
      (is (= {:error :out-of-bounds :data 10 :maximum 10 :exclusive true}
             (validate-integer-excl 10))))))

(defvalidate draft3-requires {"type" "object"
                              "properties" {"name" {"type" "string"
                                                    "required" true}
                                            "age" {"type" "integer"}}}
  {:draft3-required true})

(deftest validate-draft3-requires
  (is (nil? (draft3-requires (cheshire/parse-string "{\"name\": \"Test\"}") )))
  (is (= {:error      :properties
          :data       {"age" 42}
          :properties {"name" {:error :missing-property}}}
         (draft3-requires (cheshire/parse-string "{\"age\": 42}")))))

(defvalidate numb {"type" "number"})
(deftest validate-number
  (is (nil? (numb 3.33)))
  (is (= {:error    :wrong-type
          :expected :number
          :data     "foo"}
         (numb "foo"))))

(defvalidate valid-array {"type" "object"
                          "properties" {"things" {"type" "array"
                                                  "items" {"type" "string"}
                                                  "minItems" 3
                                                  "maxItems" 4
                                                  "uniqueItems" true}}})

(deftest validate-minimum-number-of-items
  (let [json (cheshire/parse-string "{\"things\" : [\"value\", \"value\"] }")
        errors (valid-array json)
        expected-errors {:error :properties
                         :data {"things" ["value" "value"]}
                         :properties {"things" {:error :wrong-number-of-elements
                                                :minimum 3
                                                :actual 2}}}]
    (is (= expected-errors errors))))

(deftest validate-maximum-number-of-items
  (let [json (cheshire/parse-string "{\"things\" : [\"value\", \"value\", \"value\", \"value\", \"value\"] }")
        errors (valid-array json)
        expected-errors {:error :properties
                         :data {"things" ["value" "value" "value" "value" "value"]}
                         :properties {"things" {:error :wrong-number-of-elements
                                                :maximum 4
                                                :actual 5}}}]
    (is (= expected-errors errors))))

(deftest validate-unique-items
  (let [json (cheshire/parse-string "{\"things\" : [\"value\", \"value\", \"value\", \"value\"] }")
        errors (valid-array json)
        expected-errors {:error :properties,
                         :data {"things" ["value" "value" "value" "value"]}
                         :properties {"things" {:error :duplicate-items-not-allowed
                                                :duplicates #{"value"}}}}]
    (is (= expected-errors errors))))

(deftest validate-valid-array
  (let [json (cheshire/parse-string "{\"things\" : [\"first\", \"second\", \"third\"] }")
        errors (valid-array json)]
    (is (nil? errors))))

(defvalidate enum-array {"type" "array"
                         "items" {"enum" ["foo" "bar"]}})

(deftest validate-enum-array
  (let [json (cheshire/parse-string "[\"foo\", \"kek\"]")
        errors (enum-array json)]
    (is (= errors {:error    :array-items
                   :data     ["foo" "kek"]
                   :items [{:error :invalid-enum-value
                            :data "kek"
                            :allowed-values #{"foo" "bar"}
                            :position 1}]}))))

(deftest validate-enum-array-ok
  (let [json (cheshire/parse-string "[\"foo\", \"bar\"]")
        errors (enum-array json)]
    (is (nil? errors))))

(defvalidate valid-date {"type" "object"
                         "properties" {"date" {"id" "http://jsonschema.net/date"
                                               "type" "string"
                                               "format" "date-time"}}}
  {:lax-date-time-format? true})

(deftest validate-valid-date
  (let [json (cheshire/parse-string "{\"date\": \"2015-01-30T12:00:00Z\"}")
        errors (valid-date json)]
    (is (nil? errors))))

(deftest validate-invalid-date
  (let [json (cheshire/parse-string "{\"date\": \"foo\"}")
        errors (valid-date json)
        expected-errors {:data       {"date" "foo"}
                         :error      :properties
                         :properties {"date" {:data     "foo"
                                              :error    :wrong-format
                                              :expected :date-time}}}]
    (is (= expected-errors errors))))

(defvalidate multiple-of-8 {"type" "number"
                            "multipleOf" 8})

(deftest validate-multiple-of-8
  (is (nil? (multiple-of-8 16)))
  (is (nil? (multiple-of-8 256)))
  (is (multiple-of-8 55)))

;; Definition example from validation spec
(defvalidate definitions (cheshire/parse-string "{
    \"type\": \"array\",
    \"items\": { \"$ref\": \"#/definitions/positiveInteger\" },
    \"definitions\": {
        \"positiveInteger\": {
            \"type\": \"integer\",
            \"minimum\": 0,
            \"exclusiveMinimum\": true
        }
    }
}"))

(deftest validate-definitions
  (is (nil? (definitions [1 2 3])))
  (is (= {:error :array-items
          :items [{:exclusive true :minimum 0
                   :error :out-of-bounds
                   :data -2
                   :position 1}]
          :data [1 -2 3]}
         (definitions [1 -2 3]))))
