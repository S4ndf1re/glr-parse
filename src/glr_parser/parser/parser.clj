(ns glr-parser.parser.parser
  (:require
   [clojure.set :as set]
   [com.phronemophobic.clj-graphviz :as viz]
   [glr-parser.lexer :as lex]
   [glr-parser.parser.dotted :as dot]
   [glr-parser.parser.rule :as rl]
   [glr-parser.util :refer [Ident throw-on-schema-invalid]]
   [clojure.string :as s]))

(def reserved-keywords #{:$shell})

(defn- is-zero-or-more?
  [kw]
  (s/ends-with? (name kw) "*"))

(defn- is-one-or-more?
  [kw]
  (s/ends-with? (name kw) "+"))

(defn- is-optional?
  [kw]
  (s/ends-with? (name kw) "?"))

(defn- is-repeat-or-optional?
  [kw]
  (or (is-zero-or-more? kw)
      (is-one-or-more? kw)
      (is-optional? kw)))

(defn- is-valid-input-rule-ident?
  [kw]
  (not (is-repeat-or-optional? kw)))

(def InputRuleIdent
  [:and
   #'Ident
   [:fn is-valid-input-rule-ident?]])

(def ParserBuilder
  [:map
   [:lexer #'lex/Lexer]
   [:rules [:map-of #'Ident #'rl/Rule]]])

(defn new-parser-builder
  [lexer]
  {:rules {}
   :lexer lexer})

(defn ident-exists
  [parser ident]
  (or (get-in parser [:rules ident])
      (lex/ident-exists (:lexer parser) ident)
      (contains? reserved-keywords ident)))

(defn get-rule
  [parser-builder ident]
  (if ident
    (get-in parser-builder [:rules ident])
    nil))

(defn- add-rule-unchecked
  "add a rule, without checking if the ident exists.
  This should only ever be used to insert reserved keywords as rules, for example for `:$shell`"
  [parser-builder ident rule]
  (throw-on-schema-invalid Ident ident)
  (when (get-in parser-builder [:rules ident])
    (throw (ex-info "keyword already exists in rules" {:kw ident})))
  (-> parser-builder
      (assoc-in [:rules ident] (rl/new-rule ident rule))
      (#(throw-on-schema-invalid ParserBuilder %))))

(defn- strip-last-char-from-kw
  [kw]
  (keyword (apply str (drop-last (name kw)))))

(defn- rule-from-repeat-or-optional
  [kw]
  (when-not (is-repeat-or-optional? kw)
    (throw (ex-info "kw is not repeatable or optional" {:kw kw})))
  (let [stripped-kw (strip-last-char-from-kw kw)]
    (cond
      (is-zero-or-more? kw)
      [[stripped-kw kw]
       []]
          ;;
      (is-one-or-more? kw)
      [stripped-kw (keyword (str (name stripped-kw) "*"))]
          ;;
      (is-optional? kw)
      [[stripped-kw]
       []])))

(defn- normalize-rule-alternative
  [parser-builder rule-alternative]
  (loop [pb parser-builder
         [i & is] rule-alternative
         added-rules '()]
    (if i
      (if (and (is-repeat-or-optional? i) (not (get-rule pb i)))
        (let [pb (add-rule-unchecked pb i (rule-from-repeat-or-optional i))]
          (recur pb
                 is
                 (conj added-rules (get-rule pb i))))
        (recur pb is added-rules))
      (list pb added-rules))))

(defn- normalize-rule
  [parser-builder rule]
  (loop [[r & rs] (list rule)
         pb parser-builder]
    (if r
      (let [[pb rs] (reduce (fn [[pb added-rules] rule-alternative]
                              (let [[pb to-add] (normalize-rule-alternative pb rule-alternative)]
                                (list pb (concat added-rules to-add))))
                            [pb rs]
                            (rl/rule-rules r))]
        (recur rs pb))
      pb)))

(defn add-rule
  [parser-builder ident rule]
  (throw-on-schema-invalid InputRuleIdent ident)
  (if (ident-exists parser-builder ident)
    (throw (ex-info "ident already exists" {:ident ident}))
    (let [parser-builder (add-rule-unchecked parser-builder ident rule)
          added-rule (get-rule parser-builder ident)]
      (-> parser-builder
          (normalize-rule added-rule)
          (#(throw-on-schema-invalid ParserBuilder %))))))

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
       (let [first-set (first-set-alternative parser-builder
                                              (dot/get-rest dotted-rule))]
         (if-not (contains? first-set nil)
           first-set
           (-> first-set
               (disj nil)
               (clojure.set/union (dot/get-lookahead dotted-rule)))))
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
          closure #{}
          visited #{}]
     (if v
       (let [next-rule-ident (dot/get-next v)
             parser-rule (get-rule parser next-rule-ident)]
         (if (and (not (contains? visited v)) parser-rule)
           (let [direct-closure (map-indexed
                                 (fn [idx inner]
                                   (dot/new-dotted-rule next-rule-ident
                                                        idx
                                                        inner
                                                        (follow-set-single parser v)))
                                 (rl/rule-rules parser-rule))
                 merged-closure (dot/merge-into-closure closure direct-closure)]
             (recur (concat vs (seq direct-closure)) merged-closure (conj visited v)))
           (recur vs closure (conj visited v))))
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
  (-> (dot/merge-into-closure (:head state) (:closure state))
      (group-by-ident)
      (dissoc nil)
      (#(reduce-kv (fn [acc k v]
                     (assoc acc k
                            (dot/merge-into-closure #{} (map dot/dotted-advance v))))

                   {} %))))

(defn state-get-reduces
  "Get all reduces"
  [state]
  (-> (dot/merge-into-closure (:head state) (:closure state))
      (group-by-ident)
      (get nil)
      (#(dot/merge-into-closure #{} %))))

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

(defn- is-first-state?
  [state-key]
  (seq (filter #(and (= (:dot %) 0) (= (:ident %) :$shell)) state-key)))

(defn- is-final-state?
  [state-key]
  (seq (filter #(and (= (:dot %) 1) (= (:ident %) :$shell)) state-key)))

(defn identify-accepting-state
  "Identify the accepting state, returing its key in the states list"
  [states]
  (first (filter #(is-final-state? %) (keys states))))

(defn identify-first-state
  "Identify the accepting state, returing its key in the states list"
  [states]
  (first (filter #(is-first-state? %) (keys states))))

(defn- state-to-layout
  [state]
  (let [head-str (dot/closure-to-string (:head state))
        closure-str (dot/closure-to-string (:closure state))
        id-str (str "q" (:id state))]
    (str id-str \newline
         "---" \newline
         head-str \newline
         "---" \newline
         closure-str)))

(defn- map-state-to-node
  [state start-state]
  {:label (state-to-layout state)
   :id (str "q" (:id state))
   :penwidth (if-not (= (:head state) start-state) "1" "3")})

(defn- state-to-edges
  [states state]
  (let [shifts (state-get-shifts state)]
    (mapv (fn [[k v]] (->> v
                           (get states)
                           (#(merge {:from (str "q" (:id state))
                                     :to (str "q" (:id %))
                                     :label (name k)}))))
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
                       :layout-algorithm :dot})))

(def StateId
  :int)

(def Reduce
  [:map
   [:type [:enum :reduce]]
   [:rule :keyword]
   [:variant :int]
   [:lookahead [:set :keyword]]])

(def Shift
  [:map
   [:type [:enum :shift]]
   [:next-state #'StateId]])

(def Action
  [:sequential
   [:or
    #'Reduce
    #'Shift]])

(def ActionSet
  [:map-of :keyword #'Action])

(def LR1ParserTable
  [:map
   [:start-state #'StateId]
   [:accept-state #'StateId]
   [:lexer #'lex/Lexer]
   [:actions [:map-of #'StateId #'ActionSet]]])

(defn- new-conflict-or-nil
  "Build a new conflict for a state, a shift, multiple reduces and an intersecting lookahead set
  that is valid for both the shift and all reduces."
  [state-id shift reduces lookahead-intersection]
  (cond (and (not shift) (>= (count reduces) 2)) {:type :reduce-reduce
                                                  :state state-id
                                                  :alternatives reduces
                                                  :next-token lookahead-intersection}
        (and shift (>= (count reduces) 1)) {:type :shift-reduce
                                            :state state-id
                                            :shift shift
                                            :reduces reduces
                                            :next-token lookahead-intersection}
        :else nil))

(defn- action-conflict
  [state-id token action]
  (let [shifts (filter (comp #(= % :shift) :type) action)
        shift (first shifts)
        reduces (->> action
                     (filter (comp #(= % :reduce) :type))
                     (filter (comp #(contains? % token) :lookahead)))]
    (if (> (count shifts) 1)
      (throw (ex-info "Invalid action list: two shifts appear for same token" {:state state-id}))
      (new-conflict-or-nil state-id shift reduces token))))

(defn- action-set-conflict
  [state-id action-set]
  (->> action-set
       (keys)
       (map #(action-conflict state-id % (get action-set %)))
       (filter (comp not nil?))))

(defn- find-conflicts
  "Find all conflicts. Throw an ex-info with the :type :conflict and a list of conflicts.
  Otherwise return the table"
  [table]
  (let [actions (:actions table)
        conflicts (->> actions
                       (keys)
                       (mapcat #(action-set-conflict % (get actions %))))]
    (when (seq conflicts)
      (throw (ex-info "Conflicts detected" {:type :conflict
                                            :conflicts conflicts})))
    table))

(defn- action-set-from-state
  [states state]
  (let [shifts (state-get-shifts state)
        reduces (state-get-reduces state)
        ;; For each shift, add to corresponding action
        actions (reduce (fn [acc [k v]]
                          (let [next-state (get states v)]
                            (assoc acc k (conj (get acc k []) {:type :shift
                                                               :next-state (:id next-state)}))))
                        {} shifts)
        ;; For each action, add all reduces
        actions (reduce (fn [acc [k v]]
                          (assoc acc k
                                 (reduce (fn [acc r]
                                           (conj acc
                                                 {:type :reduce
                                                  :rule (dot/get-ident r)
                                                  :variant (dot/get-variant r)
                                                  :lookahead (dot/get-lookahead r)}))
                                         v reduces)))
                        {} actions)]
    actions))

(defn to-lr-1-table
  [parser-builder start-rule-ident]
  (let [states (build-graph-states parser-builder start-rule-ident)
        accept-state (get states (identify-accepting-state states))
        start-state (get states (identify-first-state states))
        actions (loop [[s & ss] states
                       actions {}]
                  (if s
                    (recur ss (assoc actions (:id (val s)) (action-set-from-state states (val s))))
                    actions))]
    (->> {:start-state (:id start-state)
          :accept-state (:id accept-state)
          :lexer (:lexer parser-builder)
          :actions actions}
         (throw-on-schema-invalid
          LR1ParserTable)
         (find-conflicts))))
