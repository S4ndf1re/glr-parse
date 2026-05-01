(ns glr-parser.regex
  (:require [glr-parser.graph.nfa :as nfa]
            [glr-parser.graph.automaton :as autom]
            [glr-parser.graph.terminal :as term]
            [glr-parser.graph.meta :as meta]))

(defprotocol ToNfa
  (to-nfa [this graph start-state end-state meta] "Convert to a nfa subgraph"))

(defrecord Constant [constant]
  ToNfa
  (to-nfa [_this graph start-state end-state _meta]
    (autom/connect-states graph start-state end-state (term/->ConstTerminal constant))))

(defrecord Digit []
  ToNfa
  (to-nfa [_this graph start-state end-state _meta]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal \0 \9))))

(defrecord OneOrMore [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state meta]
    (let [graph (to-nfa inner-rule graph start-state end-state meta)]
      (autom/connect-states graph end-state start-state (term/->EpsilonTerminal)))))

(defrecord ZeroOrMore [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state meta]
    (let [graph (to-nfa inner-rule graph end-state start-state meta)]
      (autom/connect-states graph start-state end-state (term/->EpsilonTerminal)))))

(defrecord Optional [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state meta]
    (let [graph (to-nfa inner-rule graph start-state end-state meta)]
      (autom/connect-states graph start-state end-state (term/->EpsilonTerminal)))))

(defrecord AnyOne []
  ToNfa
  (to-nfa [_this graph start-state end-state _meta]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal (char 0) (char 255)))))

(defrecord Or [alternatives]
  ToNfa
  (to-nfa [_this graph start-state end-state meta]
    (reduce (fn [g v] (to-nfa v g start-state end-state meta)) graph alternatives)))

(defrecord Sequence [sequence]
  ToNfa
  (to-nfa [_this graph start-state end-state meta]
    ;; First, construct a list of the form (start-state, n_0, n_1, ..., n_(n-3), n_(n-2), end_state)
    (let [states-and-graph (reduce (fn [l-g, _]
                                     (let [graph (l-g :graph)
                                           states (l-g :states)
                                           [graph new-state-id] (autom/new-state graph false meta)
                                           states (conj states new-state-id)]
                                       {:states states :graph graph}))
                                   {:states (list end-state) :graph graph} ;; make sure to add end-state first, as it is a push front linked list
                                   (take (- (count sequence) 1) (range)))
          graph (states-and-graph :graph)
          states (conj (states-and-graph :states) start-state)]
      (loop [graph graph
             states states
             sequence-entry sequence]
        (if (seq sequence-entry)
          (recur (to-nfa (first sequence-entry) graph (first states) (second states) meta)
                 (rest states)
                 (rest sequence-entry))
          graph)))))

(defrecord Range [start end]
  ToNfa
  (to-nfa [_this graph start-state end-state _meta]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal start end))))

(defn- convert-to-nfa-graph
  "Convert a top level rule to an nfa graph"
  [graph ident rule precedence]
  (let [meta-obj (meta/new-meta ident precedence)
        [graph first-state] (autom/new-state graph false meta-obj)
        [graph final-state] (autom/new-state graph true meta-obj)
        graph (to-nfa rule graph first-state final-state meta-obj)]
    (list graph first-state)))

(defn build-nfa-graph
  [rules]
  (let [graph (nfa/new-nfa-graph)
        [graph first-state] (autom/new-state graph false nil)
        graph (reduce (fn [g r]
                        (let [[graph rule-start] (convert-to-nfa-graph g (:ident r) (:rule r) (:precedence r))]
                          (autom/connect-states graph first-state rule-start (term/->EpsilonTerminal))))
                      graph
                      rules)]
    (autom/set-start-state (autom/normalize graph) first-state)))

(defn add-rule
  [rules ident rule & {:keys [precedence] :or {precedence 0}}]
  (conj rules {:ident ident
               :rule rule
               :precedence precedence}))
