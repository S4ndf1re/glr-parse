(ns glr-parser.util
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(def Ident
  :keyword)

(defn throw-on-schema-invalid
  [schema value]
  (cond
    (not (m/validate schema value)) (throw
                                     (ex-info
                                      (str (me/humanize (m/explain schema value)))
                                      {:type :invalid-schema
                                       :error (m/explain schema value)}))
    :else value))

(defn fn-arities [f]
  (->> (.getDeclaredMethods (class f))
       (filter (fn [^java.lang.reflect.Method m] (= "invoke" (.getName m))))
       (map (fn [^java.lang.reflect.Method m] (alength (.getParameterTypes m))))
       distinct
       sort))
