(ns glr-parser.graph.edge-map
  (:require
   [glr-parser.graph.terminal :as term]))

(defn create-edge
  "Create a new edge of the form id (start) terminal target"
  [id terminal target]
  {:id id
   :terminal terminal
   :target target})

(defn normalize-edge
  "normalize the edge by applying attacking terminals"
  [edge attacks]
  (for [split (term/split-by-attacks (edge :terminal) attacks)]
    (create-edge (edge :id) split (edge :target))))

(defn normalize-edges
  "normalize all edges by applying attacking terminals"
  [edges attacks]
  (mapcat #(normalize-edge % attacks) edges))


(defn id
  [edge]
  (edge :id))

(defn target
  [edge]
  (edge :target))

(defn terminal
  [edge]
  (edge :terminal))
