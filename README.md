# json-schema validator

Currently contains a usable JSON schema validator using cheshire to parse JSON.
Supportes linked schemas with $ref and allows user to specify
how linked URIs are loaded.

[![Clojars Project](http://clojars.org/webjure/json-schema/latest-version.svg)](http://clojars.org/webjure/json-schema)

[![Build Status](https://travis-ci.org/tatut/json-schema.svg?branch=master)](https://travis-ci.org/tatut/json-schema)

## Status

The project is tested against [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
and passes most of the tests.

The macro version has problems with recursive and huge schemas.
A schema that later links to itself with a "#" pointer causes an ever expanding
macro expansion to take place and fails. That case requires a rethink of the
macro version. The macro may also fail when the generated function would exceed
the JVM limit on allowed code in a single method (64k).

Currently there are 46 tests with 642 assertions.
See [suite_test.clj](https://github.com/tatut/json-schema/blob/master/test/webjure/json_schema/suite_test.clj#L10) for
a the list of tests in the JSON schema test suite that are skipped.

## Usage

The function version (runtime loading of schema):

```clojure
(ns my.app
  (:require [webjure.json-schema.validator :refer [validate]]
            [cheshire.core :as cheshire]))

;;; then in some function
(validate (cheshire/parse-string json-schema)
          (cheshire/parse-string json-data))

```

Macro version loads and parses the schema and generates the validation function at compile time.
The returned errors are exactly the same as in the runtime version.

```clojure
(ns my.app
  (:require [webjure.json-schema.validator.macro :refer [make-validator]]
            [cheshire.core :as cheshire]))

(def my-schema-validator
     (make-validator (cheshire/parse-string json-schema) {}))

;; Then in some function
(my-schema-validator (cheshire/parse-string json-data))
```
