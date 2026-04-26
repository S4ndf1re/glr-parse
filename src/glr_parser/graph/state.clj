(ns glr-parser.graph.state
  (:require [glr-parser.graph.edge-map :as em]
            [glr-parser.graph.terminal :as term]))

(defprotocol AutomatonState
  (get-id [this] "get the id of the state")
  (add-connection [this target terminal] "connect state over terminal to target")
  (remove-connection [this target] "Remove the connection from this to target, for all terminals")
  (remove-connection-for-terminal [this target terminal] "remove the connection from this to target, for the single terminal")
  (get-all-terminals [this] "get a list of all terminals")
  (to-edge-list [this] "convert the state to a list of maps containting id, terminal and target, for all connections")
  (clear-edges [this] "clear all edges")
  (is-final [this] "Check if the state is final")
  (get-connections-for-symbol [this c] "Try to match the character c to the terminals in connections, returning the first match found")
  (set-meta [this meta] "Set meta data")
  (get-meta [this]))

(defrecord
 ^{:doc "NfaState is an automaton state, that allows for epsilon transitions, and allows for multiple targets for a single terminal"}
 NfaState [id connections is-final metadata]
  AutomatonState

  (get-id [_this] id)

  (add-connection [this target terminal]
    (let [connection-set (get connections terminal #{})
          connection-set (conj connection-set target)
          new-connections (assoc connections terminal connection-set)]
      (assoc this :connections new-connections)))

  (remove-connection [this target]
    (assoc this :connections
           (loop [[t & ts] (keys connections)
                  result-conns connections]
             (if t
               (let [conns-for-terminal (get result-conns t)
                     new-conns-for-terminal (remove #{target} conns-for-terminal)]
                 (if (seq new-conns-for-terminal)
                   (recur ts (assoc result-conns t new-conns-for-terminal))
                   (recur ts (dissoc result-conns t))))
               (result-conns)))))

  (remove-connection-for-terminal [this target terminal]
    (let [new-conns-for-terminal (remove #{target} (get connections terminal))]
      (if (seq new-conns-for-terminal)
        (assoc-in this [:connections terminal] new-conns-for-terminal)
        (assoc this :connections (dissoc connections terminal)))))

  (get-all-terminals [_this]
    (keys connections))

  (to-edge-list [_this]
    (for [conn connections
          target (val conn)]
      (em/create-edge id (key conn) target)))

  (clear-edges [this]
    (assoc this :connections {}))
  (is-final [_this] is-final)

  (get-connections-for-symbol [_this c]
    (let [connection-keys (filter #(term/match % c) (keys connections))]
      (if (seq connection-keys)
        (connections (first connection-keys))
        (list))))

  (set-meta [this meta]
    (assoc this :metadata meta))
  (get-meta [_this]
    metadata))

(defrecord
 ^{:doc "DfaState is a automaton state, that does not allow for epsilon terminals, and only allows for one target for a single termianl"}
 DfaState [id connections is-final metadata]
  AutomatonState
  (get-id [_this] id)

  (add-connection [this target terminal]
    (if (term/is-epsilon terminal)
      (throw (Exception. "DfaState does not allow for epsilon transitions"))

      (if (and (connections terminal) (not (= (connections terminal) target)))
        (throw (Exception. "can't add terminal->target connection, since the terminal is already assigned to another target"))
        (assoc-in this [:connections terminal] target))))

  (remove-connection [this target]
    (assoc this :connections (remove (fn [[_k v]] (= v target)) connections)))

  (remove-connection-for-terminal [this target terminal]
    (if (= (connections terminal) target)
      (assoc this :connections (dissoc connections terminal))
      this))

  (get-all-terminals [_this]
    (keys connections))

  (to-edge-list [_this]
    (map (fn [[k v]] (em/create-edge id k v)) connections))

  (clear-edges [this]
    (assoc this :connections {}))
  (is-final [_this] is-final)

  (get-connections-for-symbol [_this c]
    (let [connection-keys (filter #(term/match % c) (keys connections))]
      (if (seq connection-keys)
        (connections (first connection-keys))
        nil)))

  (set-meta [this meta]
    (assoc this :metadata meta))
  (get-meta [_this]
    metadata))
