(ns glr-parser.graph.nfa
  (:require [glr-parser.graph.state :as state]
            [glr-parser.graph.edge-map :as em]
            [glr-parser.graph.automaton :as autom]
            [glr-parser.graph.dfa :as dfa]
            [glr-parser.graph.terminal :as term]
            [glr-parser.graph.meta :as meta]
            [clojure.set]))

(defrecord NfaAutomaton [states counter is-normalized start-state]
  autom/Automaton
  (new-state [this is-final meta]
    (let [next-id (+ counter 1)
          state (state/->NfaState counter {} is-final meta)]
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

  (set-state [this st]
    (assoc-in this [:states (state/get-id st)] st))

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
           (reduce (fn [states [k s]] (assoc states k (state/clear-edges s))) states states)))

  (apply-meta [this meta]
    (assoc this :states (reduce-kv (fn [m k v] (assoc m k (state/set-meta v meta))) {} states))))

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

(defn- get-precedence-by-id
  [nfa-graph state-id]
  (let [st (autom/get-state nfa-graph state-id)]
    (if st
      (meta/precedence (state/get-meta st))
      nil)))

(defn- get-rule-by-id
  [nfa-graph state-id]
  (let [st (autom/get-state nfa-graph state-id)]
    (if st
      (meta/rule (state/get-meta st))
      nil)))

(defn- is-closure-final
  [graph closure]
  (let [final-states (sort-by #(get-precedence-by-id graph %)
                              (filter #(state/is-final (autom/get-state graph %)) closure))
        is-final (not (= (first final-states) nil))]
    (if (and (> (count final-states) 1)
             (= (get-precedence-by-id graph (first final-states)) (get-precedence-by-id graph (second final-states))))
      (throw (Exception. (str "ambiguity detected between rules '"
                              (get-rule-by-id graph (first final-states))
                              "' and '"
                              (get-rule-by-id graph (second final-states))
                              "'")))
      (list (first final-states) is-final))))

(defn- build-state-closure-for-terminal
  [nfa-graph dfa-graph states-map state-set terminal]
  (let [targets (get-transitions-for-states (map #(autom/get-state nfa-graph %) state-set) terminal)
        eps-closure (transduce (eps-closure nfa-graph) clojure.set/union #{} targets)
        [final-state is-final] (is-closure-final nfa-graph eps-closure)]
    (if (seq eps-closure)
      (if (states-map eps-closure)
        (list eps-closure dfa-graph (states-map eps-closure))
        ;; Create a new state, return the new graph and new state
        (let [[dfa-graph new-state-id] (autom/new-state dfa-graph is-final
                                                        (meta/new-meta (get-rule-by-id nfa-graph final-state)
                                                                       (get-precedence-by-id nfa-graph final-state)
                                                                       eps-closure))]
          (list eps-closure dfa-graph new-state-id)))
      (list nil dfa-graph nil))))

(defn- process-state-and-terminals
  [nfa-graph dfa-graph states-map state-set terminals]
  (loop [dfa-graph dfa-graph
         [t & ts] terminals
         states-map states-map
         created-states #{}]
    (if t
      (let [[eps-closure new-graph new-state] (build-state-closure-for-terminal nfa-graph dfa-graph states-map state-set t)]
        (if (and eps-closure new-state)
          (let [new-graph (autom/connect-states new-graph (states-map state-set) new-state t)]
            (recur new-graph
                   ts
                   (assoc states-map eps-closure new-state)
                   (conj created-states eps-closure)))
          (recur new-graph ts states-map created-states)))

      (list dfa-graph states-map created-states))))

(defn to-dfa
  "Convert the nfa graph to the nfa graph"
  [graph]
  (let [terminals (filter #(not (term/is-epsilon %)) (autom/get-all-terminals graph))
        start-state (autom/get-start-state graph)]
    (if (not start-state)
      (throw (Exception. "Start state of nfa not specified"))

      (let [start-state-eps-closure (eps-closure graph start-state)
            [final-state is-final] (is-closure-final graph start-state-eps-closure)
            dfa-graph (dfa/new-dfa)
            [dfa-graph first-state] (autom/new-state dfa-graph
                                                     is-final
                                                     (meta/new-meta
                                                      (get-rule-by-id graph final-state)
                                                      (get-precedence-by-id graph final-state)
                                                      start-state-eps-closure))
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
