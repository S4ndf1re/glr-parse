(ns glr-parser.parser.dotted
  (:require [clojure.string :as s]))

;; NOTE(jan): Rules are lists. For now. Lets see how this will change in the future.
;; NOTE(jan): if a rule has multiple alternatives, it is a list of lists

(defn- assert-1d-list
  [l]
  (if (and (seqable? l) (seqable? (first l)))
    (throw (ex-info "list cannot be 2d", {:list l}))
    l))

(defn new-dotted-rule
  ([ident rule]
   {:ident ident
    :rule (assert-1d-list rule)
    :dot 0}))

(defn get-ident
  [dotted]
  (:ident dotted))

(defn get-rule
  [dotted]
  (:rule dotted))

(defn dotted-advance [rule]
  (assoc rule :dot (inc (:dot rule))))

(defn is-at-end
  [rule]
  (= (count (:rule rule)) (:dot rule)))

(defn get-next
  [rule]
  (if-not (is-at-end rule)
    (nth (get-rule rule) (:dot rule) nil)
    nil))

(defn- dotted-rule-to-string
  [rule]
  (let [rule-str (s/join "" (map name (:rule rule)))
        rule-str (str (subs rule-str 0 (:dot rule)) "." (subs rule-str (:dot rule)))
        ident-str (name (:ident rule))]
    (str ident-str " := " rule-str)))

(defn closure-to-string
  [closure]
  (clojure.string/join "\\n" (map dotted-rule-to-string closure)))
