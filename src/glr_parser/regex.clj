(ns glr-parser.regex
  (:require [glr-parser.nfa :as nfa]
            [glr-parser.graph.automaton :as autom]
            [glr-parser.graph.terminal :as term]))

(defprotocol ToNfa
  (to-nfa [this graph start-state end-state] "Convert to a nfa subgraph"))

(defrecord Constant [constant]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (autom/connect-states graph start-state end-state (term/->ConstTerminal constant))))

(defrecord Digit []
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal \0 \9))))

(defrecord OneOrMore [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (let [graph (to-nfa inner-rule graph start-state end-state)]
      (autom/connect-states graph end-state start-state (term/->EpsilonTerminal)))))

(defrecord ZeroOrMore [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (let [graph (to-nfa inner-rule graph end-state start-state)]
      (autom/connect-states graph start-state end-state (term/->EpsilonTerminal)))))

(defrecord Optional [inner-rule]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (let [graph (to-nfa inner-rule graph start-state end-state)]
      (autom/connect-states graph start-state end-state (term/->EpsilonTerminal)))))

(defrecord AnyOne []
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal (char 0) (char 255)))))

(defrecord Or [alternatives]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (reduce (fn [g v] (to-nfa v g start-state end-state)) graph alternatives)))

(defrecord Sequence [sequence]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    ;; First, construct a list of the form (start-state, n_0, n_1, ..., n_(n-3), n_(n-2), end_state)
    (let [states-and-graph (reduce (fn [l-g, _]
                                     (let [graph (l-g :graph)
                                           states (l-g :states)
                                           new-graph-and-state (autom/new-state graph false)
                                           graph (first new-graph-and-state)
                                           states (conj states (second new-graph-and-state))]
                                       {:states states :graph graph}))
                                   {:states (list end-state) :graph graph} ;; make sure to add end-state first, as it is a push front linked list
                                   (take (- (count sequence) 1) (range)))
          graph (states-and-graph :graph)
          states (conj (states-and-graph :states) start-state)]
      (loop [graph graph
             states states
             sequence-entry sequence]
        (if (seq sequence-entry)
          (recur (to-nfa (first sequence-entry) graph (first states) (second states))
                 (rest states)
                 (rest sequence-entry))
          graph)))))

(defrecord Range [start end]
  ToNfa
  (to-nfa [_this graph start-state end-state]
    (autom/connect-states graph start-state end-state (term/->RangeTerminal start end))))

(defn convert-to-nfa-graph
  "Convert a top level rule to an nfa graph"
  [rule]
  (let [graph (nfa/new-nfa-graph)
        [graph first-state] (autom/new-state graph false)
        [graph final-state] (autom/new-state graph true)
        graph (autom/set-start-state graph first-state)]
    (autom/normalize (to-nfa rule graph first-state final-state))))



(defn test
  []
  (let [rule (->Sequence [(->OneOrMore (->Digit))
                          (->Constant \.)
                          (->ZeroOrMore (->Digit))])
        nfa-graph (convert-to-nfa-graph rule)]
    (autom/to-graphviz nfa-graph "nfa.png")
    (autom/to-graphviz (nfa/to-dfa nfa-graph) "dfa.png")))
