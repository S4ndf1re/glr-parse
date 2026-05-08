(ns glr-parser.parser.rule)

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

(defn new-rule
  [ident rule]
  {:ident ident
   :rules (rule-alternatives rule)})

(defn rule-ident
  [rule]
  (:ident rule))

(defn rule-rules
  [rule]
  (:rules rule))
