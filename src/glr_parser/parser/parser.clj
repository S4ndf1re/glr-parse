(ns glr-parser.parser.parser
  (:require
   [clojure.set :as set]
   [com.phronemophobic.clj-graphviz :as viz]
   [glr-parser.lexer :as lex]
   [glr-parser.parser.dotted :as dot]
   [glr-parser.parser.rule :as rl]))

(def reserved-keywords #{:$shell})

(defn new-parser
  [lexer]
  {:rules {}
   :lexer lexer})

(defn ident-exists
  [parser ident]
  (or (get-in parser [:rules ident])
      (lex/ident-exists (:lexer parser) ident)
      (contains? reserved-keywords ident)))

(defn- add-rule-unchecked
  "add a rule, without checking if the ident exists.
  This should only ever be used to insert reserved keywords as rules, for example for `:$shell`"
  [parser ident rule]
  (assoc-in parser [:rules ident] (rl/new-rule ident rule)))

(defn add-rule
  [parser ident rule]
  (if (ident-exists parser ident)
    (throw (ex-info "ident already exists" {:ident ident}))
    (add-rule-unchecked parser ident rule)))

(defn get-rule
  [parser ident]
  (if ident
    (get-in parser [:rules ident])
    nil))

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
       (let [next-rule-ident (dot/get-next v)
             parser-rule (get-rule parser next-rule-ident)]
         (if (and (not (contains? closure v)) parser-rule)
           (let [direct-closure (into #{} (map #(dot/new-dotted-rule next-rule-ident %)
                                               (rl/rule-rules parser-rule)))]
             (recur vs (set/union direct-closure closure)))
           (recur vs closure)))
       closure))))

(defn new-state
  "Define a new lr parser graph state. the connections are defined by the groups that can be extracted from the closure.
  A state may be identified by id after building the parser table, or by the head while building the parsert table and graph"
  [parser id head]
  (let [closure (transduce (build-closure parser) set/union #{} head)]
    {:id id
     :head head
     :closure closure}))

(defn- group-by-ident
  "Group a set of dotted rules into a map of possible next idents"
  [dotted-set]
  (->> dotted-set
       (group-by dot/get-next)
       (reduce-kv (fn [acc k v] (assoc acc k (into #{} v))) {})))

(defn state-get-shifts
  "Get all shifts, with the already advanced dotted items"
  [state]
  (-> (set/union (:head state) (:closure state))
      (group-by-ident)
      (dissoc nil)
      (#(reduce-kv (fn [acc k v]
                     (assoc acc k
                            (into #{} (map dot/dotted-advance v))))
                   {} %))))

(defn state-get-reduces
  "Get all reduces"
  [state]
  (get (group-by-ident (set/union (:head state) (:closure state))) nil))

(defn build-graph-states
  "Build the graph states, by determining the shell for the start rule, and building all shift (goto) states.
  A tuple of all states and the final state is returned. The final state beeing the shell advanced by one.
  Each state of `states` is composed of a head (the key of the states map) and a closure that can be build by resolving all dotted non-terminals.
  The Head is always a set of dotted items, directly identifying the state, hence all keys of the states map are sets of dotted items.

  NOTE: that the edges are NOT returned, as they can be quickly identified using the `(state-get-shifts state)` function"
  [parser start-rule-ident]
  (let [parser (add-rule-unchecked parser :$shell [start-rule-ident])
        rule (get-rule parser :$shell)
        shell-dotted (dot/new-dotted-rule :$shell (first (rl/rule-rules rule)))
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

(defn- is-final-state
  [state-key]
  (seq (filter #(and (= (:dot %) 1) (= (:ident %) :$shell)) state-key)))

(defn identify-accepting-state
  "Identify the accepting state, returing its key in the states list"
  [states]
  (first (filter #(is-final-state %) (keys states))))

(defn- state-to-layout
  [state]
  (let [head-str (dot/closure-to-string (:head state))
        closure-str (dot/closure-to-string (:closure state))
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
