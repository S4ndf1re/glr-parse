(ns glr-parser.parser.rule
  (:require [glr-parser.util :refer [throw-on-schema-invalid]]))

(def Ident
  [:keyword])

(def RuleList
  [:cat
   [:*
    :keyword]
   [:or
    :keyword
    [:fn fn?]]])

(def Rule
  [:or
   #'RuleList
   [:sequential #'RuleList]])

(defn- is-nested?
  "Check if the list is nested"
  [l]
  (and (seqable? l) (seqable? (first l))))

(defn rule-alternatives
  "Nest a rule if it is not nested, hence always getting a 2d list of alternatives"
  [rule]
  (if (is-nested? rule)
    (into [] rule)
    [rule]))

(defn- get-last-if-callback
  "Get the callback on the last position, or get the identity function"
  [list]
  (if (fn? (last list))
    (last list)
    identity))

(defn- get-all-except-last-if-callback
  [list]
  (if (fn? (last list))
    (drop-last list)
    list))

(defn new-rule
  [ident rule]
  (throw-on-schema-invalid Rule rule)
  (throw-on-schema-invalid Ident ident)
  (let [alternatives (rule-alternatives rule)
        callbacks (mapv get-last-if-callback alternatives)
        rules (mapv get-all-except-last-if-callback alternatives)]
    {:ident ident
     :rules rules
     :callbacks callbacks}))

(defn rule-ident
  [rule]
  (:ident rule))

(defn rule-rules
  [rule]
  (:rules rule))
