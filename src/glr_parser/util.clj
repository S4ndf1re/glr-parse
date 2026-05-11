(ns glr-parser.util
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(defn throw-on-schema-invalid
  [schema value]
  (cond
    (not (m/validate schema value)) (throw
                                     (ex-info
                                      (str (me/humanize (m/explain schema value)))
                                      {:type :invalid-schema
                                       :error (m/explain schema value)}))
    :else value))
