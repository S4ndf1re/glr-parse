(ns glr-parser.lexer-test
  (:require [glr-parser.lexer :as lex]
            [glr-parser.regex :as rgx]
            [clojure.test :refer [testing is deftest]]))

(deftest lexer-test-1
  (let [lexer (-> (lex/new-empty "abc 1234 1234.1234" "abc")
                  (lex/add-const :abc "abc")
                  (lex/add-rule :number
                                (rgx/->Sequence [(rgx/->OneOrMore (rgx/->Digit))
                                                 (rgx/->Constant \.)
                                                 (rgx/->ZeroOrMore (rgx/->Digit))]))
                  (lex/add-rule :word
                                (rgx/->OneOrMore (rgx/->Or [(rgx/->Range \a \z)
                                                            (rgx/->Range \A \Z)
                                                            (rgx/->Digit)])))
                  (lex/add-rule :whitespace
                                (rgx/->Or [(rgx/->Constant \space)
                                           (rgx/->Constant \newline)]))
                  (lex/add-skip :whitespace)
                  (lex/build))]

    (testing "expected tokens"
      (let [expected-token-idents [:abc :word :number :eof]]
        (loop [lexer lexer
               [ex & exs] expected-token-idents]
          (if ex
            (let [[lexer token] (lex/advance lexer)]
              (is (= (lex/token-ident token) ex))
              (recur lexer exs))
            nil))))))

(deftest lexer-test-strings-1
  (let [lexer (-> (lex/new-empty "\"test \\n 123\" \"cdef.hello, world \\\" \" abc 123." "abc")
                  (lex/add-const :abc "abc")
                  (lex/add-rule :number
                                (rgx/->Sequence [(rgx/->OneOrMore (rgx/->Digit))
                                                 (rgx/->Constant \.)
                                                 (rgx/->ZeroOrMore (rgx/->Digit))]))
                  (lex/add-rule :string
                                (rgx/->Sequence [(rgx/->Constant \")
                                                 (rgx/->ZeroOrMore
                                                  (rgx/->Or [(rgx/->Range \space \!)
                                                             (rgx/->Range \# \[)
                                                             (rgx/->Range \] \~)
                                                             (rgx/->Sequence [(rgx/->Constant (char 92))
                                                                              (rgx/->Or [(rgx/->Constant \\)
                                                                                         (rgx/->Constant \n)
                                                                                         (rgx/->Constant \r)
                                                                                         (rgx/->Constant \t)
                                                                                         (rgx/->Constant \")])])]))
                                                 (rgx/->Constant \")]))
                  (lex/add-rule :whitespace
                                (rgx/->Or [(rgx/->Constant \space)
                                           (rgx/->Constant \newline)]))
                  (lex/add-skip :whitespace)
                  (lex/build))]

    (testing "expected tokens"
      (let [expected-token-idents [:string :string :abc :number :eof]]
        (loop [lexer lexer
               [ex & exs] expected-token-idents]
          (if ex
            (let [[lexer token] (lex/advance lexer)]
              (is (= (lex/token-ident token) ex))
              (recur lexer exs))
            nil))))))
