(ns glr-parser.parser-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [glr-parser.lexer :as lex]
   [glr-parser.parser.parser :as par]
   [glr-parser.regex :as rgx]))

(deftest parser-test-1
  (testing "Build simple parser out of lecture. Test if works, generate graph"
    (let [lexer (-> (lex/new-empty)
                    (lex/add-const :a "a")
                    (lex/add-const :b "b")
                    (lex/add-const :c "c")
                    (lex/add-const :d "d")
                    (lex/add-const :e "e")
                    (lex/build))
          parser (-> (par/new-parser-builder lexer)
                     (par/add-rule :S [[:a :A :b :S :c :S]
                                       [:a :A :b :S]
                                       [:d]])
                     (par/add-rule :A [:e]))
          _states (par/build-graph-states parser :S)]
          ;; _ (par/to-graphviz states)]
      (try
        (par/build-lr-1 parser :S)
        (catch Exception e
          (is (= ((comp :type ex-data) e) :conflict)))))))

(deftest parser-test-2
  (testing "Build a simple arithmetic lexer, and execute on a sample string"
    (let [lexer (-> (lex/new-empty)
                    (lex/add-rule :number (rgx/->OneOrMore (rgx/->Digit)) :callback parse-long)
                    (lex/add-const :plus "+")
                    (lex/add-const :minus "-")
                    (lex/add-const :mul "*")
                    (lex/add-const :div "/")
                    (lex/add-const :l-paren "(")
                    (lex/add-const :r-paren ")")
                    (lex/add-const :semicolon ";")
                    (lex/build))
          parser (-> (par/new-parser-builder lexer)
                     (par/add-rule :S [:Statement+ (fn [[stmts]] (:data stmts))])
                     (par/add-rule :Statement [:Expr :semicolon (fn [[expr _]] (:data expr))])
                     (par/add-rule :Expr [[:Expr :plus :Term (fn [[first _ last]] (+ (:data first) (:data last)))]
                                          [:Expr :minus :Term (fn [[first _ last]] (- (:data first) (:data last)))]
                                          [:Term (fn [[first]] (:data first))]])
                     (par/add-rule :Term [[:Term :mul :Factor (fn [[first _ last]] (* (:data first) (:data last)))]
                                          [:Term :div :Factor (fn [[first _ last]] (/ (:data first) (:data last)))]
                                          [:Factor (fn [[first]] (:data first))]])
                     (par/add-rule :Factor [[:number (fn [[number]] (:data number))]
                                            [:l-paren :Expr :r-paren (fn [[_ expr _]] (:data expr))]]))
          parser-table (par/build-lr-1 parser :S)
          ;; _ (par/to-graphviz states)
          ast (par/run-lr-1 parser-table "1+2*3;" "test")]
      (is (= (:data ast) '(7))))))

(deftest parser-test-3
  (testing "Build a simple arithmetic lexer, and execute on a sample string"
    (let [lexer (-> (lex/new-empty)
                    (lex/add-rule :number (rgx/->OneOrMore (rgx/->Digit)) :callback parse-long)
                    (lex/add-const :plus "+")
                    (lex/add-const :minus "-")
                    (lex/add-const :mul "*")
                    (lex/add-const :div "/")
                    (lex/add-const :l-paren "(")
                    (lex/add-const :r-paren ")")
                    (lex/add-const :semicolon ";")
                    (lex/build))
          parser (-> (par/new-parser-builder lexer)
                     (par/add-rule :S [:Statement+ (fn [[stmts]] (:data stmts))])
                     (par/add-rule :Statement [:Expr :semicolon (fn [[expr _]] (:data expr))])
                     (par/add-rule :Expr [[:Expr :plus :Term (fn [[first _ last]] (+ (:data first) (:data last)))]
                                          [:Expr :minus :Term (fn [[first _ last]] (- (:data first) (:data last)))]
                                          [:Term (fn [[first]] (:data first))]])
                     (par/add-rule :Term [[:Term :mul :Factor (fn [[first _ last]] (* (:data first) (:data last)))]
                                          [:Term :div :Factor (fn [[first _ last]] (/ (:data first) (:data last)))]
                                          [:Factor (fn [[first]] (:data first))]])
                     (par/add-rule :Factor [[:number (fn [[number]] (:data number))]
                                            [:l-paren :Expr :r-paren (fn [[_ expr _]] (:data expr))]]))
          parser-table (par/build-lr-1 parser :S)
          ;; _ (par/to-graphviz states)
          ast (par/run-lr-1 parser-table "1+2*(7-2);" "test")]
      (is (= (:data ast) '(11))))))

(deftest parser-test-4
  (testing "Build a simple arithmetic lexer, and execute on a sample string"
    (let [lexer (-> (lex/new-empty)
                    (lex/add-rule :number (rgx/->OneOrMore (rgx/->Digit)) :callback parse-long)
                    (lex/add-const :plus "+")
                    (lex/add-const :minus "-")
                    (lex/add-const :mul "*")
                    (lex/add-const :div "/")
                    (lex/add-const :l-paren "(")
                    (lex/add-const :r-paren ")")
                    (lex/add-const :semicolon ";")
                    (lex/build))
          parser (-> (par/new-parser-builder lexer)
                     (par/add-rule :S [:Statement+ (fn [[stmts]] (:data stmts))])
                     (par/add-rule :Statement [:Expr :semicolon (fn [[expr _]] (:data expr))])
                     (par/add-rule :Expr [[:Expr :plus :Term (fn [[first _ last]] (+ (:data first) (:data last)))]
                                          [:Expr :minus :Term (fn [[first _ last]] (- (:data first) (:data last)))]
                                          [:Term (fn [[first]] (:data first))]])
                     (par/add-rule :Term [[:Term :mul :Factor (fn [[first _ last]] (* (:data first) (:data last)))]
                                          [:Term :div :Factor (fn [[first _ last]] (/ (:data first) (:data last)))]
                                          [:Factor (fn [[first]] (:data first))]])
                     (par/add-rule :Factor [[:number (fn [[number]] (:data number))]
                                            [:l-paren :Expr :r-paren (fn [[_ expr _]] (:data expr))]]))
          parser-table (par/build-lr-1 parser :S)
          ;; _ (par/to-graphviz states)
          ast (par/run-lr-1 parser-table "1+2*(7-2);(5*(2+3)-20);" "test")]
      (is (= (:data ast) '(11, 5))))))
