# json-schema tools

Currently contains only a very minimal JSON schema validator using cheshire to parse JSON.
Supportes linked schemas with $ref and allows user to specify
how linked URIs are resolved.

[![Clojars Project](http://clojars.org/webjure/json-schema/latest-version.svg)](http://clojars.org/webjure/json-schema)

## Usage

```clojure
(ns my.app
  (:require [webjure.json-schema.validator :refer [validate]]
            [cheshire.core :as cheshire]))

;;; then in some function
(validate (cheshire/parse-string json-schema)
          (cheshire/parse-string json-data))
	  
```
