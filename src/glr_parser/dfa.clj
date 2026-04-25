(ns glr-parser.dfa
  (:require
   [glr-parser.graph.automaton :as autom]
   [glr-parser.graph.edge-map :as em]
   [glr-parser.graph.state :as state]))

(defrecord DfaAutomaton [states counter is-normalized start-state]
  autom/Automaton
  (new-state [this is-final]
    (let [next-id (+ counter 1)
          state (state/->DfaState counter {} is-final)]
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

(defn new-dfa
  []
  (->DfaAutomaton {} 0 false nil))
