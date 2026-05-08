(ns glr-parser.parser.dotted
  (:require [glr-parser.parser.parser :as par]
            [glr-parser.parser.rule :as rl]
            [clojure.set :as set]
            [clojure.string :as s]
            [com.phronemophobic.clj-graphviz :as viz]))

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

(defn- inspect
  [x]
  (println "Inspect:" x)
  x)

(defn build-closure
  "Transform a dotted-rule to a closure of dotted items. If the rule is not supplied, return a transducer"
  ([parser] (fn [rf]
              (fn
                ([] (rf))
                ([acc] (rf acc))
                ([acc rule] (rf acc (build-closure parser rule))))))

  ([parser dotted-rule]
   (loop [[v & vs] (list dotted-rule)
          closure #{}]
     (if v
       (let [next-rule-ident (get-next v)
             parser-rule (par/get-rule parser next-rule-ident)]
         (if (and (not (contains? closure v)) parser-rule)
           (let [direct-closure (into #{} (map #(new-dotted-rule next-rule-ident %)
                                               (rl/rule-rules parser-rule)))]
             (recur vs (clojure.set/union direct-closure closure)))
           (recur vs closure)))
       closure))))

(defn group-by-ident
  "Group a set of dotted rules into a map of possible next idents"
  [dotted-set]
  (->> dotted-set
       (group-by get-next)
       (reduce-kv (fn [acc k v] (assoc acc k (into #{} v))) {})))

(defn new-state
  "Define a new lr parser graph state. the connections are defined by the groups that can be extracted from the closure.
  A state may be identified by id after building the parser table, or by the head while building the parsert table and graph"
  [parser id head]
  (let [closure (transduce (build-closure parser) clojure.set/union #{} head)]
    {:id id
     :head head
     :closure closure}))

(defn state-get-shifts
  "Get all shifts, with the already advanced dotted items"
  [state]
  (-> (clojure.set/union (:head state) (:closure state))
      (group-by-ident)
      (dissoc nil)
      (#(reduce-kv (fn [acc k v]
                     (assoc acc k
                            (into #{} (map dotted-advance v))))
                   {} %))))

(defn state-get-reduces
  "Get all reduces"
  [state]
  (get (group-by-ident (clojure.set/union (:head state) (:closure state))) nil))

(defn- is-final-state
  [state-key]
  (seq (filter #(and (= (:dot %) 1) (= (:ident %) :$shell)) state-key)))

(defn identify-accepting-state
  "Identify the accepting state, returing its key in the states list"
  [states]
  (first (filter #(is-final-state %) (keys states))))

(defn build-graph-states
  "Build the graph states, by determining the shell for the start rule, and building all shift (goto) states.
  A tuple of all states and the final state is returned. The final state beeing the shell advanced by one.
  Each state of `states` is composed of a head (the key of the states map) and a closure that can be build by resolving all dotted non-terminals.
  The Head is always a set of dotted items, directly identifying the state, hence all keys of the states map are sets of dotted items.

  NOTE: that the edges are NOT returned, as they can be quickly identified using the `(state-get-shifts state)` function"
  [parser start-rule-ident]
  (let [parser (par/add-rule parser :$shell [start-rule-ident])
        rule (par/get-rule parser :$shell)
        shell-dotted (new-dotted-rule :$shell (first (rl/rule-rules rule)))
        shell-rule #{shell-dotted}]
    (loop [next-id 0
           [v & vs] (list shell-rule)
           states {}]
      (if v
        (if-not (get states v)
          (let [state (new-state parser next-id v)
                shifts (state-get-shifts state)
                to-visit (reduce (fn [acc [_ v]] (conj acc v)) vs shifts)]
            (recur (inc next-id) to-visit (assoc states v state)))
          (recur next-id vs states))

        states))))

(defn- dotted-rule-to-string
  [rule]
  (let [rule-str (s/join "" (map name (:rule rule)))
        rule-str (str (subs rule-str 0 (:dot rule)) "." (subs rule-str (:dot rule)))
        ident-str (name (:ident rule))]
    (str ident-str " := " rule-str)))

(defn- closure-to-string
  [closure]
  (clojure.string/join "\\n" (map dotted-rule-to-string closure)))

(defn- state-to-layout
  [state]
  (let [head-str (closure-to-string (:head state))
        closure-str (closure-to-string (:closure state))
        id-str (str "q" (:id state))]
    (str "{ " id-str "|" head-str "|" closure-str " }")))

(defn- map-state-to-node
  [state start-state]
  {:label (state-to-layout state)
   :id (str "q" (:id state))
   :shape "record"
   :penwidth (if-not (= (:head state) start-state) "1" "3")})

(defn- state-to-edges
  [states state]
  (let [shifts (state-get-shifts state)]
    (mapv (fn [[k v]] (->> v
                           (get states)
                           (#(merge {:from (str "q" (:id state))
                                     :to (str "q" (:id %))
                                     :label (name k)
                                     :len "2"}))))
          shifts)))

(defn to-graphviz
  [states]
  (let [start-state (identify-accepting-state states)
        nodes {:nodes (map #(map-state-to-node % start-state) (vals states))}
        edges {:edges (mapcat #(state-to-edges states (val %)) states)}]
    (viz/render-graph (merge nodes edges {:default-attributes {:edge {:label ""}
                                                               :node {:label ""
                                                                      :penwidth "1"}}
                                          :flags #{:directed}})
                      {:filename "img/lr_0.png"
                       :layout-algorithm :neato})))
