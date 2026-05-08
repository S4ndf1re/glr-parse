(ns glr-parser.parser.parser
  (:require
   [glr-parser.lexer :as lex]
   [glr-parser.parser.rule :as rl]))


(defn new-parser
  [lexer]
  {:rules {}
   :lexer lexer})

(defn ident-exists
  [parser ident]
  (or (get-in parser [:rules ident]) (lex/ident-exists (:lexer parser) ident)))

(defn add-rule
  [parser ident rule]
  (if (ident-exists parser ident)
    (throw (ex-info "ident already exists" {:ident ident}))
    (assoc-in parser [:rules ident] (rl/new-rule ident rule))))

(defn get-rule
  [parser ident]
  (if ident
    (get-in parser [:rules ident])
    nil))
