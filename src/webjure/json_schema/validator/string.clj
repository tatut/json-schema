(ns webjure.json-schema.validator.string)

(defn length
  "Calculate string length in unicode codepoints"
  [str]
  (let [string-length (count str)]
    (loop [i 0
           len 0]
      (if (= i string-length)
        len
        (recur (+ i (Character/charCount (.codePointAt str i)))
               (inc len))))))
