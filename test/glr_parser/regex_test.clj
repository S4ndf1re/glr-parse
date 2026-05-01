(ns glr-parser.regex-test
  (:require [glr-parser.regex :as rgx]
            [glr-parser.graph.automaton :as autom]
            [glr-parser.nfa :as nfa]
            [glr-parser.dfa :as dfa]
            [clojure.test :refer [testing is deftest]]))

(deftest regex-to-nfa-to-dfa-to-exec-test
  (let [rules (-> ()
                  (rgx/add-rule :number
                                (rgx/->Sequence [(rgx/->OneOrMore (rgx/->Digit))
                                                 (rgx/->Constant \.)
                                                 (rgx/->ZeroOrMore (rgx/->Digit))])))
        nfa-graph (rgx/build-nfa-graph rules)
        dfa-graph (nfa/to-dfa nfa-graph)]

    (autom/to-graphviz nfa-graph "img/nfa.png")
    (autom/to-graphviz dfa-graph "img/dfa.png")

    (testing "dfa matches 1234.5678"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.5678")]
        (is (= (longest :length) (count "1234.5678")))
        (is (= (longest :rule) :number))
        (is (= overall true))))

    (testing "dfa matches 1234."
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.")]
        (is (= (longest :length) (count "1234.")))
        (is (= (longest :rule) :number))
        (is (= overall true))))

    (testing "dfa does not match .1234"
      (let [[longest overall] (dfa/execute-dfa dfa-graph ".1234")]
        (is (= longest nil))
        (is (= overall false))))

    (testing "dfa matches partially 1234.1234abc"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.1234abc")]
        (is (= (longest :length) (count "1234.1234")))
        (is (= (longest :rule) :number))
        (is (= overall false))))))

(deftest regex-to-nfa-to-dfa-to-exec-test-2
  (let [rules (-> ()
                  (rgx/add-rule :number
                                (rgx/->Sequence [(rgx/->OneOrMore (rgx/->Digit))
                                                 (rgx/->Constant \.)
                                                 (rgx/->ZeroOrMore (rgx/->Digit))]))
                  (rgx/add-rule :word
                                (rgx/->OneOrMore (rgx/->Or [(rgx/->Range \a \z)
                                                            (rgx/->Range \A \Z)
                                                            (rgx/->Digit)]))))
        nfa-graph (rgx/build-nfa-graph rules)
        dfa-graph (nfa/to-dfa nfa-graph)]

    (autom/to-graphviz nfa-graph "img/nfa2.png")
    (autom/to-graphviz dfa-graph "img/dfa2.png")

    (testing "dfa matches 1234.5678"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.5678")]
        (is (= (longest :length) (count "1234.5678")))
        (is (= (longest :rule) :number))
        (is (= overall true))))

    (testing "dfa matches 1234."
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.")]
        (is (= (longest :length) (count "1234.")))
        (is (= (longest :rule) :number))
        (is (= overall true))))

    (testing "dfa does not match .1234"
      (let [[longest overall] (dfa/execute-dfa dfa-graph ".1234")]
        (is (= longest nil))
        (is (= overall false))))

    (testing "dfa matches partially 1234.1234abc"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.1234abc")]
        (is (= (longest :length) (count "1234.1234")))
        (is (= (longest :rule) :number))
        (is (= overall false))))

    (testing "dfa matches 'abc' fully"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "abc")]
        (is (= (longest :length) (count "abc")))
        (is (= (longest :rule) :word))
        (is (= overall true))))

    (testing "dfa matches 'abc123' fully"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "abc123")]
        (is (= (longest :length) (count "abc123")))
        (is (= (longest :rule) :word))
        (is (= overall true))))

    (testing "dfa matches '123abc' fully"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "123abc")]
        (is (= (longest :length) (count "123abc")))
        (is (= (longest :rule) :word))
        (is (= overall true))))))
