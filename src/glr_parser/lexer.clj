(ns glr-parser.lexer
  (:require
   [glr-parser.regex :as rgx]
   [glr-parser.nfa :as nfa]
   [clojure.string :as s]
   [glr-parser.dfa :as dfa]
   [glr-parser.graph.automaton :as autom]))

(defn new-empty
  "Build a new lexer, that accepts both the consts and rules. Skip rules that are contained in skips by name"
  [input-string filename]
  {:consts {}
   :rules {}
   :rules-graph nil
   :skips #{}
   :current-idx 0
   :input-string (vec input-string)
   :filename filename})

(defn add-const
  [lexer ident constant]
  (if (get-in lexer [:consts ident])
    (throw (ex-info "constant already exists" {:type :const-exists :ident ident :constant constant}))
    (assoc-in lexer [:consts ident] {:ident ident
                                     :constant (clojure.string/trim constant)
                                     :length (count (clojure.string/trim constant))})))

(defn add-rule
  [lexer ident rule & {:keys [precedence] :or {precedence 0}}]
  (if (get-in lexer [:rules ident])
    (throw (ex-info "rule already exists" {:type :rule-exists :ident ident :rule rule}))
    (assoc-in lexer [:rules ident] {:ident ident :rule rule :precedence precedence})))

(defn add-skip
  [lexer ident]
  (assoc lexer :skips (conj (:skips lexer) ident)))

(defn- duplicate-consts
  [lexer]
  (->> lexer
       :consts
       (map :constant)
       (frequencies)
       (remove (comp #{1} val))
       (map key)))

(defn build
  [lexer]
  (let [duplicates (duplicate-consts lexer)
        nfa-graph (rgx/build-nfa-graph (vals (:rules lexer)))
        dfa-graph (nfa/to-dfa nfa-graph)]
    (autom/to-graphviz dfa-graph "debug_dfa.png")
    (if (seq duplicates)
      (throw (ex-info "duplicate constants found" {:type :duplicate-consts
                                                   :duplicates duplicates}))
      (-> lexer
          (assoc :rules-graph dfa-graph)))))

(defn- current-input
  [lexer]
  (subvec (:input-string lexer) (:current-idx lexer)))

(defn- starts-with?
  "Custom string starts-with? to use with persistend vector"
  [input test]
  (loop [[t & ts] test
         [i & is] input]
    (cond
      (and t i (= t i)) (recur ts is)
      (and (not t) i) true
      :else false)))

(defn- get-longest-match
  [input const-longest-match dfa-longest-match]
  (cond
    (and (not const-longest-match) (not dfa-longest-match))
    (throw (ex-info "cannot match next token"
                    {:type :no-applicable-rule
                     :next-word (clojure.string/join "" input)}))

    (and const-longest-match (not dfa-longest-match))
    (list (:length const-longest-match)
          (:ident const-longest-match))

    (and (not const-longest-match) dfa-longest-match)
    (list (:length dfa-longest-match)
          (:rule dfa-longest-match))

    (and const-longest-match dfa-longest-match (> (:length dfa-longest-match) (count (:constant const-longest-match))))
    (list (:length dfa-longest-match)
          (:rule dfa-longest-match))

    (and const-longest-match dfa-longest-match (<= (:length dfa-longest-match) (count (:constant const-longest-match))))
    (list (:length const-longest-match)
          (:ident const-longest-match))

    :else (throw (ex-info "CRITICAL: all cases checked already" {}))))

(defn- new-token
  [ident value start end]
  {:ident ident
   :value (apply str value)
   :start start
   :end end})

(defn token-range
  "Get the start-end range in the form [start, end) for a token"
  [tok]
  (list (:start tok) (:end tok)))

(defn token-ident
  "Get the token identifier as specified by the rule"
  [tok]
  (:ident tok))

(defn token-value
  "Get the value as a string. Note that the string conversion from vector of chars to string is performed in the new-token private function"
  [tok]
  (:value tok))

(defn- peek-with-length
  "Peek a token, also return the length the lexer would have to advance, to land behind the token. This is also the length of the matched token"
  [lexer]
  (if (not (seq (current-input lexer)))
    (list 0 :eof)
    (let [current-input-vec (current-input lexer)
          matching-const (first (sort-by #(- (:length %))
                                         (filter #(starts-with? current-input-vec (:constant %))
                                                 (vals (:consts lexer)))))
          [longest-match _] (dfa/execute-dfa (:rules-graph lexer) current-input-vec)
          [advance-by matched-token] (get-longest-match current-input-vec matching-const longest-match)]
      (list advance-by matched-token))))

(defn- advance-lexer-to-idx
  [lexer next-idx]
  (assoc lexer :current-idx next-idx))

(defn advance
  "Advance the lexer by one token, returning both the lexer and the token. If the additional parameter n is supplied, advance n times.
  If the advanced token is part of the skips set, skip the token and return the logical next token by recursively calling into advance"
  ([lexer]
   (let [[match-length token] (peek-with-length lexer)
         start (:current-idx lexer)
         end (+ start match-length)]
     (if ((:skips lexer) token)
       (advance (advance-lexer-to-idx lexer end))
       (list (advance-lexer-to-idx lexer end)
             (new-token token (subvec (:input-string lexer) start end) start end)))))
  ([lexer n]
   (loop [n n
          tokens []
          lexer lexer]
     (if (> n 0)
       (let [[lex, tok] (advance lexer)]
         (recur (dec n) (conj tokens tok) lex))
       (list lexer
             (into '() tokens))))))

#_{:clj-kondo/ignore [:redefined-var]}
(defn peek
  "Peek the next token, by executing advance, but not returning the changed lexer.
  An additional parameter n can be supplied, so that n tokens are peeked and returned list"
  ([lexer] (second (advance lexer)))
  ([lexer n] (second (advance lexer n))))
