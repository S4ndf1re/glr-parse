(ns glr-parser.core-test
  (:require [clojure.test :refer :all]
            [glr-parser.regex :as rgx]
            [glr-parser.nfa :as nfa]
            [glr-parser.dfa :as dfa]))

(deftest regex-to-nfa-to-dfa-to-exec-test
  (let [rule (rgx/->Sequence [(rgx/->OneOrMore (rgx/->Digit))
                              (rgx/->Constant \.)
                              (rgx/->ZeroOrMore (rgx/->Digit))])
        nfa-graph (rgx/convert-to-nfa-graph rule)
        dfa-graph (nfa/to-dfa nfa-graph)]

    (testing "dfa matches 1234.5678"
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.5678")]
        (is (= longest (count "1234.5678")))
        (is (= overall true))))

    (testing "dfa matches 1234."
      (let [[longest overall] (dfa/execute-dfa dfa-graph "1234.")]
        (is (= longest (count "1234.")))
        (is (= overall true))))

    (testing "dfa does not match .1234"
      (let [[longest overall] (dfa/execute-dfa dfa-graph ".1234")]
        (is (= longest nil))
        (is (= overall false))))))
