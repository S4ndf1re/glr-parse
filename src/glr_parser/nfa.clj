(ns glr-parser.nfa
  (:require [glr-parser.graph.state :as state]
            [glr-parser.graph.edge-map :as em]
            [glr-parser.graph.automaton :as autom]
            [glr-parser.dfa :as dfa]
            [glr-parser.graph.terminal :as term]
            [clojure.set]))

(defrecord NfaAutomaton [states counter is-normalized start-state]
  autom/Automaton
  (new-state [this is-final]
    (let [next-id (+ counter 1)
          state (state/->NfaState counter {} is-final)]
      (list (-> this
                (assoc :counter next-id)
                (assoc :states (assoc states (state/get-id state) state)))
            (state/get-id state))))

  (set-start-state [this state]
    (assoc this :start-state state))

  (get-start-state [_this]
    start-state)

  (get-state [_this id]
    (get states id))

  (get-states [_this]
    (vals states))

  (connect-states [this start end terminal]
    (let [state (get states start)
          state (state/add-connection state end terminal)
          new-states (assoc states start state)]
      (-> this
          (assoc :states new-states)
          (assoc :is-normalized false))))

  (unconnect-states [this start end]
    (let [state (autom/get-state this start)]
      (if state
        (assoc-in this [:states start] (state/remove-connection state end))
        this)))

  (unconnect-states-for-terminal [this start end terminal]
    (let [state (autom/get-state this start)]
      (if state
        (assoc-in this [:states start] (state/remove-connection-for-terminal state end terminal))
        this)))

  (get-all-terminals [_this]
    (flatten (for [state (vals states)]
               (state/get-all-terminals state))))

  (to-edge-list [_this]
    (mapcat state/to-edge-list (vals states)))

  (normalize [this]
    (let [terminals (autom/get-all-terminals this)
          edges (autom/to-edge-list this)
          normalized-edges (em/normalize-edges edges terminals)
          graph (autom/clear-edges this)]
      (assoc (reduce
              (fn [g e]
                (autom/connect-states
                 g
                 (em/id e)
                 (em/target e)
                 (em/terminal e)))
              graph
              normalized-edges)
             :is-normalized true)))

  (clear-edges [this]
    (assoc this :states
           (reduce (fn [states [k s]] (assoc states k (state/clear-edges s))) states states))))

(defn new-nfa-graph
  "Creates a new NFA with epsilon transitions.
  Epsilon transitions are used for simplicity"
  []
  (->NfaAutomaton {} 0 false nil))

(defn terminal-filter-map
  [terminal]
  (comp
   (filter #(= (em/terminal %) terminal))
   (map #(em/target %))))

(defn- get-transitions-for-states
  "Get all transition states for a set of states and a terminal, as a set"
  [states terminal]
  (transduce (comp
              (mapcat state/to-edge-list)
              (terminal-filter-map terminal))
             conj
             #{}
             states))

(defn- eps-closure
  "Get the epsilon closure of a state (passed by id), i.e. all states that are reachable to any depth using only epsilon transitions.
  When the start state is not specified, create a transducer"
  ([nfa-graph]
   (fn [rf]
     (fn
       ([] rf)
       ([result] (rf result))
       ([result state]
        (rf result (eps-closure nfa-graph state))))))

  ([nfa-graph start-state-id]
   (loop [[to-visit-id & to-visit-rest] (list start-state-id)
          visited-ids #{}]
     (println to-visit-id)
     (if to-visit-id
       (if (not (get visited-ids to-visit-id))
         (recur (transduce (comp
                            (filter #(term/is-epsilon (em/terminal %)))
                            (map em/target))
                           conj
                           to-visit-rest
                           (state/to-edge-list (autom/get-state nfa-graph to-visit-id)))
                (conj visited-ids to-visit-id))
         (recur to-visit-rest visited-ids))
       visited-ids))))

(defn- build-state-closure-for-terminal
  [nfa-graph dfa-graph states-map state-set terminal]
  (let [targets (get-transitions-for-states (map #(autom/get-state nfa-graph %) state-set) terminal)
        eps-closure (transduce (eps-closure nfa-graph) clojure.set/union #{} targets)
        is-final (reduce (fn [f s] (or f (state/is-final (autom/get-state nfa-graph s)))) false eps-closure)]
    (println "Targets: " targets)
    (if (states-map eps-closure)
      (list eps-closure dfa-graph (states-map eps-closure))
      ;; Create a new state, return the new graph and new state
      (let [[dfa-graph new-state] (autom/new-state dfa-graph is-final)]
        (list eps-closure dfa-graph new-state)))))

(defn- process-state-and-terminals
  [nfa-graph dfa-graph states-map state-set terminals]
  (loop [dfa-graph dfa-graph
         [t & ts] terminals
         states-map states-map
         created-states #{}]
    (if t
      (let [[eps-closure new-graph new-state] (build-state-closure-for-terminal nfa-graph dfa-graph states-map state-set t)
            new-graph (autom/connect-states new-graph (states-map state-set) new-state t)]
        (recur new-graph
               ts
               (assoc states-map eps-closure new-state)
               (conj created-states eps-closure)))

      (list dfa-graph states-map created-states))))

(defn to-dfa
  "Convert the nfa graph to the nfa graph"
  [graph]
  (let [terminals (filter #(not (term/is-epsilon %)) (autom/get-all-terminals graph))
        start-state (autom/get-start-state graph)]
    (if (not start-state)
      (throw (Exception. "Start state of nfa not specified"))

      (let [start-state-eps-closure (eps-closure graph start-state)
            dfa-graph (dfa/new-dfa)
            [dfa-graph first-state] (autom/new-state dfa-graph false)
            dfa-graph (autom/set-start-state dfa-graph first-state)]
        ;; Loop as long as there is a state that is not visited
        (loop [dfa-graph dfa-graph
               states-map {start-state-eps-closure first-state}
               [current & to-process] (list start-state-eps-closure)
               visited #{}]
          (if current
            (if (not (visited current))
              ;; Happy path, process current and connect to new states
              (let [[dfa-graph states-map created-states] (process-state-and-terminals graph dfa-graph states-map current terminals)]
                (recur dfa-graph states-map (apply conj to-process created-states) (conj visited current)))
              (recur dfa-graph states-map to-process visited))
            dfa-graph))))))
