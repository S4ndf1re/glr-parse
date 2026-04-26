(ns glr-parser.graph.automaton
  (:require
   [com.phronemophobic.clj-graphviz :as viz]
   [glr-parser.graph.terminal :as term]
   [glr-parser.graph.state :as state]
   [glr-parser.graph.edge-map :as em]
   [glr-parser.graph.meta :as meta]
   [clojure.string]))

(defprotocol Automaton
  (new-state [this is-final meta] "Create a new state, returning this and the new state")
  (set-start-state [this state] "set the start state id")
  (get-start-state [this] "get the start state id")
  (get-state [_this id] "Get the state with id")
  (set-state [_this state] "set the state with the states id")
  (get-states [_this] "get a list of states")
  (connect-states [this start end terminal] "Connect start and end using the terminal, form a new connection (edge)")
  (unconnect-states [this start end] "Unconnect the states start and end, for all terminals")
  (unconnect-states-for-terminal [this start end terminal] "unconnect start and end for the specified terminal")
  (get-all-terminals [this] "get a list of all terminals")
  (to-edge-list [this] "Convert to a list of id, terminal target maps for each state")
  (normalize [this] "normalize graph")
  (clear-edges [this] "clear all edges")
  (apply-meta [this meta] "apply the metadata to all states"))

(defn- map-state-id-to-node-id
  "Produce \"q<n>\", where <n> is the parameter state-id.
  This can later be used for identifying graphviz nodes by id"
  [state-id]
  (str "q" state-id))

(defn- map-states-to-nodes
  "Map a list of states, each beeing a map, to a list of graphviz nodes.
  Applies final state marking and naming"
  [states]
  (map (fn [s] {:id (map-state-id-to-node-id (state/get-id s))
                :peripheries (if (state/is-final s) "2" "1")
                :label (if (and (state/get-meta s)
                                (meta/closure (state/get-meta s)))
                         (clojure.string/join "," (map map-state-id-to-node-id (seq (meta/closure (state/get-meta s)))))
                         (map-state-id-to-node-id (state/get-id s)))}) states))

(defn to-graphviz
  "Render the graph to graphviz"
  [graph filename]
  (let [edges (to-edge-list graph)
        nodes {:nodes (map-states-to-nodes (get-states graph))}
        edges (map (fn [e] {:from (map-state-id-to-node-id (em/id e))
                            :to (map-state-id-to-node-id (em/target e))
                            :label (term/to-label (em/terminal e))})
                   edges)]
    (viz/render-graph (merge nodes {:edges edges} {:default-attributes {:edge {:label ""}
                                                                        :node {:label ""}}
                                                   :flags #{:directed}})
                      {:filename filename})))
