(ns webjure.json-schema.ref
  (:require [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn initialize-id-path
  "Recursively set the id path in the schema for subsequent reference loading."
  ([schema] (initialize-id-path "" schema))
  ([prefix schema]
   (let [id-path (str prefix (get schema "id"))]
     (with-meta (reduce-kv
                 (fn [m k v]
                   (assoc m k
                          (if (map? v)
                            (initialize-id-path id-path v)
                            v)))
                 {}
                 schema)
       {::id-path id-path}))))

(defn resolve-ref [uri]
  (-> uri
      slurp
      cheshire/parse-string
      initialize-id-path))

(def pointer-pattern #"^#/.*")
(defn pointer? [ref]
  (re-matches pointer-pattern ref))

(defn- unescape [c]
  (-> c
      (str/replace #"~1" "/")
      (str/replace #"~0" "~")
      java.net.URLDecoder/decode))

(defn dereference-pointer [root-schema ref]
  (let [components  (-> ref
                        (subs 2 (count ref))
                        (str/split #"/"))
        key-path (mapv (fn [c]
                         (let [c (unescape c)]
                           (if (re-matches #"\d+" c)
                             (Long/parseLong c)
                             c)))
                       components)]
    ;;(println "REF: " ref " => key path: " (pr-str key-path) "    root-schema: " root-schema)
    (get-in root-schema key-path)))


(defn- ref? [schema]
  (contains? schema "$ref"))

(defn resolve-schema [schema options]
  (loop [schema schema
         root-schema (:root-schema options)]
    (let [ref (get schema "$ref")]
      ;;(println "RESOLVE-SCHEMA, ref: " (pr-str ref) ", schema: " (pr-str schema) ", root: " (pr-str root-schema))
      (cond

        ;; No reference, return as is
        (nil? ref)
        (do #_(println " => " schema) schema)

        ;; Reference to whole document
        (= ref "#")
        (do #_(println " => " root-schema) root-schema)

        ;; Pointer, dereference it
        (pointer? ref)
        (recur (dereference-pointer root-schema ref) root-schema)

        ;; URI, try to load it
        :default
        (let [id-path (::id-path (meta schema))
              uri (str id-path ref)
              fragment (.getFragment (java.net.URI. uri))
              resolver (or (:ref-resolver options)
                           resolve-ref)
              referenced-schema (resolver uri)
              referenced-schema (if (str/blank? fragment)
                                  referenced-schema
                                  ;; If there was a fragment treat it as a
                                  ;; $ref within the loaded document
                                  (resolve-schema {"$ref" (str "#" fragment)}
                                                  {:root-schema referenced-schema}))]
          (if-not referenced-schema
            ;; Throw exception if schema can't be loaded
            (throw (IllegalArgumentException.
                    (str "Unable to resolve referenced schema: " ref)))
            (recur referenced-schema referenced-schema)))))))

(defn root-schema [{root :root-schema :as options} schema]
  (if root
    options
    (assoc options
           :root-schema (or (resolve-schema schema options)
                            schema))))
