(ns glr-parser.lexer
  (:require
   [clojure.string :as s]
   [glr-parser.common.token :refer [new-token]]
   [glr-parser.graph.automaton :as autom]
   [glr-parser.graph.dfa :as dfa]
   [glr-parser.graph.nfa :as nfa]
   [glr-parser.regex :as rgx]
   [glr-parser.util :refer [Ident throw-on-schema-invalid fn-arities]]))

(def reserved-keywords #{:eof})

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

(def LexerType
  [:map
   [:consts [:map-of #'Ident #'InnerConst]]
   [:rules [:map-of #'Ident #'InnerRule]]
   [:callbacks [:map-of #'Ident #'Callback]]
   [:rules-graph [:maybe #'autom/AutomatonType]]
   [:skips [:set #'Ident]]
   [:current-idx :int]
   [:input-string [:vector char?]]
   [:filename [:maybe :string]]])

(defprotocol ILexer
  (start-lexing [this input-string filename] "Instruct the lexer to start from the beginning for input string and filename")
  (advance [this] "Advance the lexer by one. Use advance-n to call this multiple times and get a list of tokens")
  #_{:clj-kondo/ignore [:redefined-var]}
  (peek [this] "Peek the next lexer token. Use peek-n to call this multiple times")
  (ident-exists [this ident] "return true, if the ident is alread registered within the lexer, to avoid duplicate tokens in the parser"))

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
         (#(throw-on-schema-invalid LexerType %))))))

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
         (#(throw-on-schema-invalid LexerType %))))))

(defn add-skip
  "Add a rule or constant to the skip list"
  [lexer ident]
  (throw-on-schema-invalid Ident ident)
  (-> lexer
      (assoc :skips (conj (:skips lexer) ident))
      (#(throw-on-schema-invalid LexerType %))))

(defn- call-callback
  [lexer ident location raw-content]
  (let [callback (get-in lexer [:callbacks ident])]
    (if callback
      (if (some #{2} (fn-arities callback))
        (callback location raw-content)
        (callback raw-content))
      raw-content)))

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
          (#(throw-on-schema-invalid LexerType %))))))

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
      (not t) true
      :else false)))

(defn- get-longest-match
  [input const-longest-match dfa-longest-match]
  (cond
    (and (not const-longest-match) (not dfa-longest-match))
    (throw (ex-info (str "cannot match next token. Next: " (clojure.string/join "" input))
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

(defn- peek-with-length
  "Peek a token, also return the length the lexer would have to advance, to land behind the token. This is also the length of the matched token"
  [lexer]
  (if-not (seq (current-input lexer))
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

(defn advance-n
  [lexer n]
  (loop [n n
         tokens []
         lexer lexer]
    (if (> n 0)
      (let [[lex, tok] (advance lexer)]
        (recur (dec n) (conj tokens tok) lex))
      (list lexer
            (into '() tokens)))))

(defn peek-n
  [lexer n]
  (second (advance-n lexer n)))

(defrecord Lexer [consts rules callbacks rules-graph skips current-idx input-string filename]
  ILexer

  (advance [this]
    (let [[match-length token] (peek-with-length this)
          start current-idx
          end (+ start match-length)
          token-as-str (apply str (subvec input-string start end))
          transformed-token (call-callback this token {:start start :end end :filename filename} token-as-str)]
      (if (get skips token)
        (advance (advance-lexer-to-idx this end))
        (list (advance-lexer-to-idx this end)
              (new-token token
                         (apply str (subvec input-string start end))
                         transformed-token
                         filename
                         start end)))))

  (peek [this]
    (second (advance this)))

  (start-lexing [this input-string filename]
    (-> this
        (assoc :filename filename)
        (assoc :input-string (vec input-string))
        (assoc :current-idx 0)))

  (ident-exists [_this ident]
    (or (get consts ident) (get rules ident) (contains? reserved-keywords ident))))

(defn new-empty
  "Build a new lexer, that accepts both the consts and rules. Skip rules that are contained in skips by name"
  []
  (->Lexer
   {}
   {}
   {}
   nil
   #{}
   0
   []
   nil))
