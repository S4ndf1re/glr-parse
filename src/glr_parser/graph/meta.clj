(ns glr-parser.graph.meta)

(defn new-meta
  ([]
   {:rule nil
    :precedence 0})
  ([rule]
   {:rule rule
    :precedence 0})
  ([rule precedence]
   {:rule rule
    :precedence precedence})
  ([rule precedence closure]
   {:rule rule
    :precedence precedence
    :closure closure}))

(defn rule
  [meta]
  (:rule meta))

(defn precedence
  [meta]
  (:precedence meta))

(defn closure
  [meta]
  (:closure meta))
