(ns glr-parser.parser-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [glr-parser.lexer :as lex]
   [glr-parser.parser.parser :as par]))

(deftest parser-test-1
  (testing "Build simple parser out of lecture. Test if works, generate graph"
    (let [lexer (-> (lex/new-empty "" "")
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
          states (par/build-graph-states parser :S)
          _ (par/to-graphviz states)
          lr1-parser-table (par/to-lr-1-table parser :S)])))
