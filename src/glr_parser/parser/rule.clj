(ns glr-parser.parser.rule
  (:require [glr-parser.util :refer [throw-on-schema-invalid Ident]]
            [malli.core :as m]
            [glr-parser.parser.precedence :refer [Precedence PrecedenceOrNil Associativity PrecedenceAssociativityTuple]]))

(def RuleAlternative
  [:cat
   [:? #'PrecedenceAssociativityTuple]
   [:* :keyword]
   [:? [:fn fn?]]])

(def StrictRuleAlternative
  [:vector :keyword])

(def StrictRuleList
  [:vector #'StrictRuleAlternative])

(def StrictCallbacks
  [:vector [:fn fn?]])

(def RuleList
  [:or
   #'RuleAlternative
   [:sequential #'RuleAlternative]])

(def Rule
  [:map
   [:ident #'Ident]
   [:rules #'StrictRuleList]
   [:callbacks #'StrictCallbacks]
   [:precedences [:vector #'PrecedenceOrNil]]
   [:associativities [:vector #'Associativity]]])

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

(defn get-variant
  "Get the variant or nil, if variant is out of bounds"
  [rule variant]
  (if (>= variant (count (:rules rule)))
    nil
    (nth (:rules rule) variant)))

(defn call-callback
  [rule variant data]
  (let [callback (nth (:callbacks rule) variant)]
    (if callback
      (callback data)
      (identity data))))

(defn- get-last-if-callback
  "Get the callback on the last position, or get the identity function"
  [list]
  (if (fn? (last list))
    (last list)
    (fn [l] (map :data l))))

(defn- get-all-except-last-if-callback
  [list]
  (if (fn? (last list))
    (into [] (drop-last list))
    list))

(defn- split-precedence-associativity-rules
  [alternative]
  (cond
    (and (m/validate Precedence (first alternative))
         (m/validate Associativity (second alternative)))
    (list (first alternative) (second alternative) (subvec alternative 2))
    (and (m/validate Precedence (first alternative))
         (not (m/validate Associativity (second alternative))))
    (list (first alternative) :none (subvec alternative 1))
    :else (list nil :none alternative)))

(defn new-rule
  [ident rule]
  (throw-on-schema-invalid RuleList rule)
  (throw-on-schema-invalid Ident ident)
  (let [alternatives (rule-alternatives rule)
        callbacks (mapv get-last-if-callback alternatives)
        rules (mapv get-all-except-last-if-callback alternatives)
        splitted-rules (mapv split-precedence-associativity-rules rules)
        [precedences associativities rules] (apply map vector splitted-rules)]
    (throw-on-schema-invalid Rule {:ident ident
                                   :rules rules
                                   :callbacks callbacks
                                   :precedences precedences
                                   :associativities associativities})))

(defn rule-ident
  [rule]
  (:ident rule))

(defn rule-rules
  [rule]
  (:rules rule))

(defn get-last-terminal-or-nil
  [rule variant is-terminal?]
  (-> rule
      (get-in [:rules variant])
      (#(filter is-terminal? %))
      (last)))

(defn get-variant-precedence-and-associativiy
  [rule variant]
  [(get-in rule [:precedences variant])
   (get-in rule [:associativities variant])])
