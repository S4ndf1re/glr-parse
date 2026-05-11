(ns glr-parser.parser.parser
  (:require
   [clojure.set :as set]
   [com.phronemophobic.clj-graphviz :as viz]
   [glr-parser.lexer :as lex]
   [glr-parser.parser.dotted :as dot]
   [glr-parser.parser.rule :as rl]))

(def reserved-keywords #{:$shell})

(defprotocol LR-Parser
  (validate [this] "validate the parser, checking if all gotos are part of either the lexer or the parser")
  (parse-til-eof [this]))

(defn new-parser-builder
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
  [parser-builder ident rule]
  (assoc-in parser-builder [:rules ident] (rl/new-rule ident rule)))

(defn add-rule
  [parser-builder ident rule]
  (if (ident-exists parser-builder ident)
    (throw (ex-info "ident already exists" {:ident ident}))
    (add-rule-unchecked parser-builder ident rule)))

(defn get-rule
  [parser-builder ident]
  (if ident
    (get-in parser-builder [:rules ident])
    nil))

(declare first-set)

(defn- first-set-alternative
  "Build the first set out of a single vector of keywords (i.e. a single rule).
  If no rule alternative is provided, a transducer is returned instead"
  ([parser-builder] (fn [rf]
                      (fn
                        ([] (rf))
                        ([acc] (rf acc))
                        ([acc rule-alternative] (rf acc (first-set-alternative parser-builder rule-alternative))))))
  ([parser-builder alternative-list]
   (let [first-elem (first alternative-list)]
     (if (get-rule parser-builder first-elem)
       (first-set parser-builder first-elem)
       #{first-elem}))))

(defn first-set
  "collect all terminals that are at first position of the rule.
  The rule is extracted from the parser-builder and is expected to be a non terminal.
  The rule-ident specifies the rule in question"
  [parser-builder rule-ident]
  (let [rule (get-rule parser-builder rule-ident)]
    (if rule
      (transduce (first-set-alternative parser-builder) clojure.set/union #{} (rl/rule-rules rule))
      nil)))

(defn- follow-set-single
  "Build the follow set after the dotted item for a single dotted item.
  If no dotted item is provided, return a transducer instead"
  ([parser-builder]
   (fn [rf]
     (fn
       ([] (rf))
       ([acc] (rf acc))
       ([acc curr] (rf acc (follow-set-single parser-builder curr))))))
  ([parser-builder dotted-rule]
   (let [dotted-rule (dot/dotted-advance dotted-rule)]
     (if-not (dot/is-at-end? dotted-rule)
       (first-set-alternative parser-builder
                              (dot/get-rest dotted-rule))
       (dot/get-lookahead dotted-rule)))))

(defn follow-set
  "Build the follow set for multiple dotted rules. The follow set is build for the dotted position"
  [parser-builder dotted-rules]
  (transduce (follow-set-single parser-builder) clojure.set/union #{} dotted-rules))

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
           (let [direct-closure (map-indexed
                                 (fn [idx inner]
                                   (dot/new-dotted-rule next-rule-ident
                                                        idx
                                                        inner
                                                        (follow-set-single parser v)))
                                 (rl/rule-rules parser-rule))]
             (recur vs (dot/merge-into-closure closure direct-closure)))
           (recur vs closure)))
       closure))))

(defn new-state
  "Define a new lr parser graph state. the connections are defined by the groups that can be extracted from the closure.
  A state may be identified by id after building the parser table, or by the head while building the parsert table and graph"
  [parser id head]
  (let [closure (transduce (build-closure parser) dot/merge-into-closure #{} head)]
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
  (-> (dot/merge-into-closure (:head state) (into [] (:closure state)))
      (group-by-ident)
      (dissoc nil)
      (#(reduce-kv (fn [acc k v]
                     (assoc acc k
                            (dot/merge-into-closure #{} (map dot/dotted-advance v))))

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
  [parser-builder start-rule-ident]
  (let [parser-builder (add-rule-unchecked parser-builder :$shell [start-rule-ident])
        rule (get-rule parser-builder :$shell)
        shell-dotted (dot/new-dotted-rule :$shell 0 (first (rl/rule-rules rule)) #{:eof})
        shell-rule #{shell-dotted}]
    (loop [next-id 0
           [v & vs] (list shell-rule)
           states {}]
      (if v
        (if-not (get states v)
          (let [state (new-state parser-builder next-id v)
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
                      {:filename "img/lr_0.png"})))

(def LR1ParserTable
  [:map
   [:start-state :int]
   [:lexer :any]
   []])

(defn to-lr-1
  [parser-builder start-rule-ident]
  (let [states (build-graph-states parser-builder start-rule-ident)]))
