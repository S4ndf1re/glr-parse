(ns glr-parser.parser.dotted
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [glr-parser.util :refer [throw-on-schema-invalid]]))

;; NOTE(jan): Rules are lists. For now. Lets see how this will change in the future.
;; NOTE(jan): if a rule has multiple alternatives, it is a list of lists

(def Rule-1d
  [:vector :keyword])

(def DottedItem
  [:map
   [:ident :keyword]
   [:variant :int]
   [:rule #'Rule-1d]
   [:dot :int]
   [:lookahead [:set :keyword]]])

(defn- assert-1d-list
  [l]
  (if (and (seqable? l) (seqable? (first l)))
    (throw (ex-info "list cannot be 2d", {:list l}))
    l))

(defn new-dotted-rule
  [ident variant rule lookahead]
  (let [dotted-rule {:ident ident
                     :variant variant
                     :rule (assert-1d-list rule)
                     :dot 0
                     :lookahead lookahead}]
    (throw-on-schema-invalid DottedItem dotted-rule)))

(defn get-ident
  [dotted]
  (:ident dotted))

(defn get-variant
  [dotted]
  (:variant dotted))

(defn get-rule
  [dotted]
  (:rule dotted))

(defn is-at-end? [rule]
  (>= (:dot rule) (count (:rule rule))))

(defn dotted-advance [rule]
  (if-not (is-at-end? rule)
    (assoc rule :dot (inc (:dot rule)))
    rule))

(defn get-next
  [rule]
  (if-not (is-at-end? rule)
    (nth (get-rule rule) (:dot rule) nil)
    nil))

(defn get-rest
  "get the rest of the rule, starting at the dotted item as a vec, return empty vector when the rule is at the end"
  [rule]
  (if-not (is-at-end? rule)
    (subvec (get-rule rule) (:dot rule))
    []))

(defn get-lookahead
  [rule]
  (:lookahead rule))

(defn- dotted-rule-to-string
  [rule]
  (let [rule-str (str/join "" (map name (:rule rule)))
        rule-str (str (subs rule-str 0 (:dot rule)) "." (subs rule-str (:dot rule)))
        ident-str (name (:ident rule))]
    (str ident-str " := " rule-str "\\| \\{ " (str/join "," (map name (:lookahead rule))) " \\}")))

(defn closure-to-string
  [closure]
  (clojure.string/join "\\n" (map dotted-rule-to-string closure)))

(defn- dotted-no-lookahead
  "Convert a dotted item to a dotted item without lookahead"
  [dotted]
  {:ident (:ident dotted)
   :variant (:variant dotted)
   :rule (:rule dotted)
   :dot (:dot dotted)})

(defn- normalize-dotted
  [dotted]
  (vector (dotted-no-lookahead dotted) (:lookahead dotted)))

(defn merge-into-closure
  "merge a closure set with additional dotted items. If the same alternative occurs multiple times, merge the lookaheads"
  ([] #{})
  ([closure] closure)
  ([closure dotted-items]
   (loop [[d & ds] (seq dotted-items)
          closures (reduce (fn [acc [k v]] (assoc acc k v)) {} (map normalize-dotted closure))]
     (if d
       (let [[key lookahead] (normalize-dotted d)
             closure-lookahead (get closures key)
             lookahead (clojure.set/union lookahead closure-lookahead)
             closures (assoc closures key lookahead)]
         (recur ds closures))
       (reduce conj #{} (map (fn [[k v]] (assoc k :lookahead v)) closures))))))
