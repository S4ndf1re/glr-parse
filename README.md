# glr_parser

TODO

## Installation

TODO

## Usage

The LR(1) Parser is split into two parts. For one, the Lexer can be used to transform an input string into a token stream. This tokenstream may be used indirectly by providing the lexer into the LR(1) parser generator.

### Lexer

One of the most important parts of Parsing is the Lexer. Hence this Library comes bundled with a Lexer implementation based on RegEx, constants and NFA/DFA graphs for Token matching.
Generally, the lexer operates on the following logic:
Overall the longest match is returned as a token. A differentiation is made for constants (added with `add-const`) and rules (`add-rule`). Rules are always added as RegEx's. When both a rule or a constant matches with the same length (for example the string `'if'` would match both the constant `if` and the rule `'[a-zA-Z]*'`), the constant is the prefered match. During the conversion process from RegEx -> NFA -> DFA, states that would accept more than rule are filtered out and result in an error. In this case, precedence must be defined. Lower number indicate higher precedence. When a conflict would normally occur, the rule with the highest precedence is selected. In states where no conflict occurs, but a rule is configured with precedence, the precedence takes no effect. In addition to constants and rules, skip identifiers may be configured. When the lexer finds a token that is part of the skiplist, the advance is called repeatedly until the next non skip token is identified. This can be usefull for Commends or whitespaces, though both must be defined as a rule or constant, first. A full example with both skips, constants and rules can be found below.

```clojure
  (let [lexer (-> (lex/new-empty)
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
                  (lex/build)
                  (lex/start-lexing "abc 1234 1234.1234" "<filename>"))]
    (let [expected-token-idents [:abc :word :number :eof]]
    (loop [lexer lexer
            [ex & exs] expected-token-idents]
        (if ex
        (let [[lexer token] (lex/advance lexer)]
            (is (= (tok/ident token) ex))
            (recur lexer exs))
        nil))))
```

#### Custom Lexer

For cases where the performance of the bundled lexer is not enough, possibly due to its generalizability or poor development choices, a custom lexer may be supplied by implementing the ILexer interface for the custom Lexer. Make sure that both peek and advance return the correct token type as defined in `glr-parser.common.token.clj`. The last token that must be returned must be of ident `:eof`. All further tokens after `:eof` must also be `:eof` and must not result in program crashes.
TODO: Lexer

#### Notes on RegEx

The regex to NFA conversion is not fully RegEx compatible. Some features are missing. Below is a list of RegEx that are working, albeit with WIP syntax:

- [x] Constants
- [x] Constant Ranges
- [x] Digits
- [x] Any
- [x] Sequences
- [x] Or
- [x] Zero or more
- [x] One or more
- [x] Optional
- [ ] Not

### Parser

Currently, the parser operates in LR(1) mode. No other modes are currently supported, though it is planned to actually use General LR parsing as the main parser. The parser uses a lexer and rules to build a parsing table. Precedences may be defined, to resolve shit/reduce or reduce/reduce conflicts. In the below example, you can see how to create a simple parser based on a simple lexer.

```clojure
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
          ast (par/run-lr-1 parser-table "1+2*3;" "test")])
```

This might look confusing at first, but the general concept is rather simple. the function `(par/add-rule <parser> <rule-ident> <alternatives or single alternative>)` adds a single rule to the parser builder and returns said parser builder. The rule ident must be any keyword, that is not `:$shell`, as this is a reserved keyword for the generator (checked during insert, will lead to an exception). A single rule alternative is a simple clojure vector. The elements of the vector are identifiers to keywords of either the parser rules or the lexer. When lexer keywords are used, they can be understood as terminals. Parser rule idents represent non-terminals. The last value in the rule alternative vector can optionally be a function. This function is the callback that is called once a rule alternative is matched by the parser. In there, custom transformation logic, like mathematical operators (see the above example), can be applied. The function expectes a single parameter as a list of tokens.

Tokens of the lexer and parser share the same structure:

```clojure
{:ident ident
    :start start
    :end end
    :raw-data value
    :data transformed}
```

The `ident` is the keyword that identifies the terminal or non-terminal that was matched and is represented by this token. `start` and `end` provide the range of the matched token. For terminals (i.e. Lexer tokens), this is simply the start and end range of the string that was matched. For non-terminals (i.e. parser tokens), this is a accumulated range over all tokens that are part of the matched rules. For example, if the rule `:Expr => :Expr :plus :Term` would match, the `start` value from the `:Expr` and the `end` value from `:Term` would be used, resulting in a `:Expr`. `raw-data` contains the raw matched values, i.e. all raw tokens for non-terminals, or the matched string of terminals. When callbacks are supplied to either the non-terminal or terminal, the value returned by the callback is stored in data. Otherwise, data contains the same value as raw-data `(identity raw-data)`. Using these tokens and callbacks, the above examples `:data` would result in: `'(7)`.

#### Repeat and Optionals

Identifiers of non-terminals must adhere to the following regex: `[a-zA-Z_][\w-]*`. However, sometimes it is useful to repeat rules up to infinite times. Hence kleene star and kleene plus may be used to describe repeatability of rules. For zero or more repeatability, add a `*` to the identifier used in the rules alternative, for example `:Statement*`. Note that this is still a valid clojure keyword, but it does not pass the regex for rule identifiers. In this example, the rule `:Statement` would be matched zero or more times (kleene star). For one or more repeatabiliy, kleene plus can be used. In this case, add the `+` symbol to the end of the rule identifier lik `:Statement+`. If the matched rule should only appear optionally once, the questionmark `?` may be appended, like so: `:Statement?`. Currently it is not possibly to group multiple identifiers and repeat that group. For that, use a new rule that groups these identifiers.

Since kleene start, kleene plus and optionals are builtin features, builtin callbacks are supplied. These callbacks transform the list of matched tokens into a list of `:data`, so that custom logic can directly include the collected data of the repeated sequences. Those collections are, like other matched rules as well, provided as tokens.

#### Precedence

The above example is rather complex, due to the need for manual precedence specification. This is commonly found in LR-grammars in the form of `Expr->Term->Factor->value or parentheses`. If this pattern is not applied, parser conflicts like shift/reduce occur. In this simple case, there are two downsides. First, this introduces a lot of overhead. The goal is simple. Provide rules for `:Expr + :Expr | :Expr * :Expr | etc.`, while also respecting precedence. This syntax overhead might confuse some readers of grammar, depending on the depth of the precedences. The bigger downside is the huge bloat that this style of grammar introduces on the size of the parser table. Since lookahead is respected in LR(1) parsing, parser tables using this scheme are ginormous, leading to both slower parsing performance, and more memory usage during both parsing and storing of the parser. Hence in some cases, precedence should be configureable, that gives the generator hints when to shift and when to reduce. For this, precedences may be defined for a rule alternative by simply using a number as the first alternative value. In addition to the precedence, associativity can be assigned, that act as a fallback in case the precedence of two rules is identical. For associativity, `:left`, `:right` and `:none` may be used. The default associativity is `:none`, meaning that if two same precedence rules result in a conflict, the parser will not allow a reduce nor shift, resulting in an error.

The precedences work by assiging a precedence to a rules alternative, in addition to an associativity (left/right/none).

Applying the precedence and `:left` associativity to the parser from the previous example, we get the following:

```clojure
(deftest parser-test-5
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
                    (lex/add-rule :whitespace (rgx/->Or [(rgx/->Constant \space)
                                                         (rgx/->Constant \newline)
                                                         (rgx/->Constant \t)
                                                         (rgx/->Constant \r)]))
                    (lex/add-skip :whitespace)
                    (lex/build))
          parser (-> (par/new-parser-builder lexer)
                     (par/add-rule :S [:Statement+ (fn [[stmts]] (:data stmts))])
                     (par/add-rule :Statement [:Expr :semicolon (fn [[expr _]] (:data expr))])
                     (par/add-rule :Expr [[0 :left :number (fn [[number]] (:data number))]
                                          [0 :none :l-paren :Expr :r-paren (fn [[_ expr _]] (:data expr))]
                                          [1 :left :Expr :mul :Expr (fn [[first _ last]] (* (:data first) (:data last)))]
                                          [1 :left :Expr :div :Expr (fn [[first _ last]] (/ (:data first) (:data last)))]
                                          [2 :left :Expr :plus :Expr (fn [[first _ last]] (+ (:data first) (:data last)))]
                                          [2 :left :Expr :minus :Expr (fn [[first _ last]] (- (:data first) (:data last)))]]))
          ;; [_ states] (par/build-graph-states parser :S)
          parser-table (par/build-lr-1 parser :S)
          ;; _ (par/to-graphviz states)
          ast (par/run-lr-1 parser-table "1+2*(7-2);
                                          (5*(2+3)-20);
                                          3 + 5 * (10 - 4);
                                          (8 - 3) * (12 / 4) + 6;
                                          45 / (5 * (4 - 1)) + 7;
                                          ((14 + 6) * 2) - 15;
                                          9 * 8 - (24 / (2 + 4));
                                          (35 / 7) * (18 - 14) + 9;
                                          60 / (15 - (3 * 3)) * 2;
                                          4 * (12 - 7) + 36 / 6;
                                          (2 + 3) * (4 + 5) - (6 + 7);
                                          100 - (4 * (5 + 15)) / 2;" "test")]
      (is (= (:data ast) '(11, 5, 33, 21, 10, 25, 68, 29, 20, 26, 32, 60))))))
```

as can be seen, this is a way simpler, shorter and more concise setup, that parses exactly the same rules, while also creating a very small parser table. Note that for the precedence, just as for the lexer, lower numbers mean higher precedence.

One important note is, that precedence is not inner rule specific, meaning that if precedence is defined in another rule a conflict might be accidentally resolved, though a generator error was expected by the developer. For example, lets let `:Expr 0` (Expression alternative 0) have precedence 0, while `:Term 0` might have a precedence of 1. In this case, even though it was not planned by the developer to resolve conflicts regarding `:Term` and `:Expr`, `:Expr` will always be preferred by the parser.

## Todos

While the LR(1) parser generally works, a few quality of life improvements are planned!

- [ ] Parse regex from string to avoid annoying and error prone regex creation via expressions
- [ ] implement a grammar for generating clojure grammars. This should work similar to the lalrpop generation of parsers, i.e. allow callbacks directly within the parsed grammar file

All ToDos should actually be implemented entirely with this parser!

## License

Copyright © 2026 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
<https://www.eclipse.org/legal/epl-2.0>.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at <https://www.gnu.org/software/classpath/license.html>.
