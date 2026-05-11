(ns glr-parser.lexer
  (:require
   [glr-parser.regex :as rgx]
   [glr-parser.graph.nfa :as nfa]
   [clojure.string :as s]
   [glr-parser.graph.dfa :as dfa]
   [glr-parser.graph.automaton :as autom]
   [glr-parser.util :refer [throw-on-schema-invalid]]))

(def reserved-keywords #{:eof})

(def Ident
  :keyword)

(def Const
  :string)

(def InnerConst
  [:map
   [:ident #'Ident]
   [:constant #'Const]
   [:length :int]])

(def Rule
  #'rgx/RegEx)

(def InnerRule
  [:map
   [:ident #'Ident]
   [:rule #'Rule]
   [:precedence :int]])

(def Callback
  [:fn (fn [x] (fn? x))])

(def Lexer
  [:map
   [:consts [:map-of #'Ident #'InnerConst]]
   [:rules [:map-of #'Ident #'InnerRule]]
   [:callbacks [:map-of #'Ident #'Callback]]
   [:rules-graph [:maybe #'autom/AutomatonType]]
   [:skips [:set #'Ident]]
   [:current-idx :int]
   [:input-string [:vector char?]]
   [:filename :string]])

(defn new-empty
  "Build a new lexer, that accepts both the consts and rules. Skip rules that are contained in skips by name"
  [input-string filename]
  {:consts {}
   :rules {}
   :callbacks {}
   :rules-graph nil
   :skips #{}
   :current-idx 0
   :input-string (vec input-string)
   :filename filename})

(defn ident-exists
  [lexer ident]
  (or (get-in lexer [:consts ident]) (get-in lexer [:rules ident]) (contains? reserved-keywords ident)))

(defn add-const
  "Add a new constant, ensuring priority over rules for equal length matches"
  ([lexer ident constant]
   (add-const lexer ident constant identity))
  ([lexer ident constant callback]
   (throw-on-schema-invalid Const constant)
   (throw-on-schema-invalid Callback callback)
   (throw-on-schema-invalid Ident ident)
   (if (ident-exists lexer ident)
     (throw (ex-info "constant already exists" {:type :const-exists :ident ident :constant constant}))
     (-> lexer
         (assoc-in [:consts ident] {:ident ident
                                    :constant (clojure.string/trim constant)
                                    :length (count (clojure.string/trim constant))})
         (assoc-in [:callbacks ident] callback)
         (#(throw-on-schema-invalid Lexer %))))))

(defn add-rule
  "Add a new rule, consisting of a regex. When both a constant and regex rule match with the same lenght, the constant has priority. Otherwise, the longest match is chosen"
  ([lexer ident rule & {:keys [precedence callback] :or {precedence 0
                                                         callback identity}}]
   (throw-on-schema-invalid Rule rule)
   (throw-on-schema-invalid Callback callback)
   (throw-on-schema-invalid Ident ident)
   (if (ident-exists lexer ident)
     (throw (ex-info "rule already exists" {:type :rule-exists :ident ident :rule rule}))
     (-> lexer
         (assoc-in [:rules ident] {:ident ident :rule rule :precedence precedence})
         (assoc-in [:callbacks ident] callback)
         (#(throw-on-schema-invalid Lexer %))))))

(defn add-skip
  "Add a rule or constant to the skip list"
  [lexer ident]
  (throw-on-schema-invalid Ident ident)
  (-> lexer
      (assoc :skips (conj (:skips lexer) ident))
      (#(throw-on-schema-invalid Lexer %))))

(defn call-callback
  [lexer ident raw-content]
  (if (get-in lexer [:callbacks ident])
    ((get-in lexer [:callbacks ident] identity) raw-content)
    raw-content))

(defn- duplicate-consts
  [lexer]
  (->> lexer
       :consts
       (map (comp :constant val))
       (frequencies)
       (remove (comp #{1} val))
       (map key)))

(defn build
  "Build the lexer by applying all rules into a nfa that is then converted to a dfa. This dfa can be used to match all rules, detecting ambiguity during nfa->dfa conversion.
  After building, the lexer is ready to advance or peek"
  [lexer]
  (let [duplicates (duplicate-consts lexer)
        nfa-graph (rgx/build-nfa-graph (vals (:rules lexer)))
        dfa-graph (nfa/to-dfa nfa-graph)]
    (if (seq duplicates)
      (throw (ex-info "duplicate constants found" {:type :duplicate-consts
                                                   :duplicates duplicates}))
      (-> lexer
          (assoc :rules-graph dfa-graph)
          (#(throw-on-schema-invalid Lexer %))))))

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
  [lexer ident value start end]
  {:ident ident
   :value (apply str value)
   :start start
   :end end
   :data (call-callback lexer ident (apply str value))})

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
             (new-token lexer token (subvec (:input-string lexer) start end) start end)))))
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
