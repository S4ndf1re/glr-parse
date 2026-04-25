(ns glr-parser.rule)

(defn matcher-is-const
  [_matcher]
  true)

(defn new-terminals
  "create a map of new terminals"
  []
  {})

(defn add-terminal
  [mt t matcher]
  (assoc mt t {:matcher matcher :is-const (matcher-is-const matcher)}))


(defn remove-terminal
  "remove from terminal t from terminals mt"
  [mt t]
  (remove mt t))
