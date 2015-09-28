(ns webjure.json-schema.validator-test
  (:require [clojure.test :refer [deftest testing is]]
            [webjure.json-schema.validator :refer [validate]]
            [cheshire.core :as cheshire]))


(defn p [resource-path]
  (->> resource-path
       (str "test/resources/")
       slurp cheshire/parse-string))

(deftest validate-address-and-phone
  (testing "jsonschema.net example schema"
    (let [s (p "address-and-phone.schema.json")]
      (testing "valid json returns nil errors"
        (is (nil? (validate s (p "address-and-phone-valid.json")))))

      (testing "missing property error is reported"
        (let [e (validate s (p "address-and-phone-city-and-code-missing.json"))]
          (is (= :properties (:error e)))
          ;; "city" is reported as missing because it is required
          (is (= :missing-property (get-in e [:properties "address" :properties "city" :error])))

          ;; no errors in "phoneNumber" because missing "code" in first item is not required
          (is (nil? (get-in e [:properties "phoneNumber"])))))

      (testing "additional properties are reported"
        (is (= {:error          :additional-properties
                :property-names #{"youDidntExpectMe" "orMe"}}
               (validate s (p "address-and-phone-additional-properties.json"))))))))

(deftest validate-referenced-schema
  (testing "person schema that links to address and phone schema"
    (let [s (p "person.schema.json")]
      (testing "valid json returns nil errors"
        (is (nil? (validate s (p "person-valid.json")))))
      (testing "linked schema errors are reported"
        (is (= :missing-property
               (get-in (validate s (p "person-invalid.json"))
                       [:properties "contact" :properties "phoneNumber" :error])))))))

(deftest validate-enum
  (testing "enum values are checked"
    (let [s (cheshire/parse-string "{\"type\": \"string\", \"enum\": [\"foo\", \"bar\"]}")]
      (is (nil? (validate s "foo")))
      (is (= {:error          :invalid-enum-value
              :data           "xuxu"
              :allowed-values #{"foo" "bar"}}
             (validate s "xuxu"))))))


(deftest validate-boolean
  (testing "boolean values are checked"
    (let [s (cheshire/parse-string "{\"type\": \"boolean\"}")]
      (is (nil? (validate s true)))
      (is (nil? (validate s false)))
      (is (every? #(= :wrong-type (:error %))
                  (map #(validate s %)
                       ["foo" 42 [4 5] {"name" "Test"}]))))))

(deftest validate-integer-bounds
  (testing "integer bounds are checked"
    (testing "non exclusive bounds work"
      (let [s (cheshire/parse-string "{\"type\": \"integer\", \"minimum\": 1, \"maximum\": 10}")]
        (testing "valid values return nil"
          (is (every? nil?
                      (map #(validate s %)
                           (range 1 1)))))

        (testing "too low values report error with minimum"
          (is (= {:error :out-of-bounds :data 0 :minimum 1 :exclusive false}
                 (validate s 0))))

        (testing "too high values report error with maximum"
          (is (= {:error :out-of-bounds :data 11 :maximum 10 :exclusive false}
                 (validate s 11))))))
    (testing "exclusive bounds works"
      (let [s (cheshire/parse-string "{\"type\": \"integer\", \"minimum\": 1, \"maximum\": 10, \"exclusiveMinimum\": true, \"exclusiveMaximum\": true}")]
        (is (nil? (validate s 2)))
        (is (nil? (validate s 9)))
        (is (= {:error :out-of-bounds :data 1 :minimum 1 :exclusive true}
               (validate s 1)))
        (is (= {:error :out-of-bounds :data 10 :maximum 10 :exclusive true}
               (validate s 10)))))))

(deftest validate-draft3-requires
  (let [s (cheshire/parse-string (str "{\"type\": \"object\", \"properties\": "
                                      "{\"name\": {\"type\": \"string\", \"required\": true}, "
                                      "\"age\": {\"type\": \"integer\"}}}"))]
    (is (nil? (validate s (cheshire/parse-string "{\"name\": \"Test\"}") {:draft3-required true})))
    (is (= {:error      :properties
            :data       {"age" 42}
            :properties {"name" {:error :missing-property}}}
           (validate s (cheshire/parse-string "{\"age\": 42}") {:draft3-required true})))))

(deftest validate-number
  (is (nil? (validate (cheshire/parse-string "{\"type\": \"number\"}")
                      3.33)))
  (is (= {:error    :wrong-type
          :expected :number
          :data     "foo"}
         (validate (cheshire/parse-string "{\"type\": \"number\"}")
                   "foo"))))

(deftest validate-minimum-number-of-items
  (let [schema (cheshire/parse-string "{\"type\" : \"object\", \"properties\" : {\"things\" : {\"type\" : \"array\", \"items\" : {\"type\" : \"string\" }, \"minItems\" : 3, \"maxItems\" : 4 } } }")
        json (cheshire/parse-string "{\"things\" : [\"value\", \"value\"] }")
        errors (validate schema json {:draft3-required true})
        expected-errors {:error :properties, :data {"things" ["value" "value"]}, :properties {"things" {:error :wrong-number-of-elements, :minimum 3, :actual 2}}}]
    (is (= expected-errors errors))))

(deftest validate-maximum-number-of-items
  (let [schema (cheshire/parse-string "{\"type\" : \"object\", \"properties\" : {\"things\" : {\"type\" : \"array\", \"items\" : {\"type\" : \"string\" }, \"minItems\" : 3, \"maxItems\" : 4 } } }")
        json (cheshire/parse-string "{\"things\" : [\"value\", \"value\", \"value\", \"value\", \"value\"] }")
        errors (validate schema json {:draft3-required true})
        expected-errors {:error :properties, :data {"things" ["value" "value" "value" "value" "value"]}, :properties {"things" {:error :wrong-number-of-elements, :maximum 4, :actual 5}}}]
    (is (= expected-errors errors))))

(deftest validate-unique-items
  (let [schema (cheshire/parse-string "{\"type\" : \"object\", \"properties\" : {\"things\" : {\"type\" : \"array\", \"items\" : {\"type\" : \"string\" }, \"minItems\" : 3, \"maxItems\" : 4, \"uniqueItems\": true } } }")
        json (cheshire/parse-string "{\"things\" : [\"value\", \"value\", \"value\", \"value\"] }")
        errors (validate schema json {:draft3-required true})
        expected-errors {:error :properties, :data {"things" ["value" "value" "value" "value"]}, :properties {"things" {:error :duplicate-items-not-allowed}}}]
    (is (= expected-errors errors))))

(deftest validate-valid-array
  (let [schema (cheshire/parse-string "{\"type\" : \"object\", \"properties\" : {\"things\" : {\"type\" : \"array\", \"items\" : {\"type\" : \"string\" }, \"minItems\" : 3, \"maxItems\" : 4, \"uniqueItems\": true } } }")
        json (cheshire/parse-string "{\"things\" : [\"first\", \"second\", \"third\"] }")
        errors (validate schema json {:draft3-required true})]
    (is (nil? errors))))