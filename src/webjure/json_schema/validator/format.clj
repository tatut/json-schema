(ns webjure.json-schema.validator.format
  (:require [clj-time.format :as time-format]
            [clj-time.coerce :as time-coerce]))

(def rfc3339-formatter (time-format/formatters :date-time))

(def hostname-pattern
  ;; Courtesy of StackOverflow http://stackoverflow.com/a/1420225
  #"^(?=.{1,255}$)[0-9A-Za-z](?:(?:[0-9A-Za-z]|-){0,61}[0-9A-Za-z])?(?:\.[0-9A-Za-z](?:(?:[0-9A-Za-z]|-){0,61}[0-9A-Za-z])?)*\.?$")

(def ipv4-pattern
  #"^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$")

(def ipv6-pattern
  #"^[:a-f0-9]+$")

(def email-pattern
  ;; I know, this isn't too permissive. This just checks
  ;; that the email has an @ character (exactly one) and something
  ;; on both sides of it.
  #"^[^@]+@[^@]+$")

(defn validate-date-time [d]
  (try
    (time-format/parse rfc3339-formatter (str d))
    nil
    (catch Exception _
      {:error :wrong-format :expected :date-time :data d})))

(defn validate-lax-date-time [d]
  (when (nil? (time-coerce/from-string (str d)))
    {:error :wrong-format :expected :date-time :data d}))

(defn validate-hostname [d]
  (if (re-matches hostname-pattern (str d))
    nil
    {:error :wrong-format :expected :hostname :data d}))

(defn validate-ipv4 [d]
  (let [[ip & parts] (re-matches ipv4-pattern (str d))]
    (if (and ip
             (every? #(<= 0 (Integer/parseInt %) 255) parts))
      nil
      {:error :wrong-format :expected :ipv4 :data d})))

;; Check that the string contains only the allowed characters
;; and contains an ':' character. Then try to parse with Java
;; Inet6Address class.
(defn validate-ipv6 [d]
  (let [d (str d)]
    (if (and (re-matches ipv6-pattern d)
             (.contains d ":")
             (try
               (java.net.Inet6Address/getByName d)
               true
               (catch Exception e#
                 false)))
      nil
      {:error :wrong-format :expected :ipv6 :data d})))

(defn validate-uri [d]
  (let [d (str d)]
    (if-not (and (.contains d "/")
                 (try
                   (java.net.URI. d)
                   true
                   (catch java.net.URISyntaxException e#
                     false)))
      {:error :wrong-format :expected :uri :data d}
      nil)))

(defn validate-email [d]
  (if-not (re-matches email-pattern (str d))
    {:error :wrong-format :expected :email :data d}
    nil))
