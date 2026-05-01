(ns glr-parser.graph.terminal
  (:require
   [glr-parser.graph.interval :as intv]))

(defprotocol Terminal
  (to-interval [this] "Converts to int range [a,b], where both a and b are integers")
  (to-label [this] "convert the terminal to a label")
  (normalize [this] "normalize, so that one element ranges are actually a const instead of a range")
  (match [this c] "Returns true, if matches, :ignore if char is ignored (epsilon), false otherwise"))

(defrecord EpsilonTerminal []
  Terminal
  (to-interval [_this] [-1 -1])
  (to-label [_this] "ε")
  (normalize [this] this)
  (match [_this _c] :ignore))

(defrecord ConstTerminal [const]
  Terminal
  (to-interval [_this] [(int const) (int const)])
  (to-label [_this] (if (= \\ const)
                      "\\\\"
                      (str const)))
  (normalize [this] this)
  (match [_this c] (= const c)))

(defrecord RangeTerminal [a b]
  Terminal
  (to-interval [_this] [(int a) (int b)])
  (to-label [_this] (str "[" (if (= \\ a) "\\\\" a) "-" (if (= \\ b) "\\\\" b) "]"))
  (normalize [this] (if (and (= a b) (> a 0))
                      (->ConstTerminal a)
                      this))
  (match [_this c] (and (>= (int c) (int a))
                        (<= (int c) (int b)))))

(defn new-terminal-from-interval
  "expects [start end] as ints, converts into terminal.
  If either start or end is < 0, meaning out of ascii space, a new epsilon transition is created"
  [interval]
  (cond
    (or (< (first interval) 0) (< (second interval) 0)) (->EpsilonTerminal)
    (= (first interval) (second interval)) (->ConstTerminal (char (first interval)))
    :else (->RangeTerminal (char (first interval)) (char (second interval)))))

(defn split-by-attacks
  "\"Attack\" the target by splitting the target,
  when a interval is partially or fully contained, do not split,
  if is equal, or the target is fully included in the attack.
  Returns a list of new intervals without intersection,
  that guarantess, that all other intervals are including
  or are equal to the new interval"
  [term attacks]
  (let [term-intv (to-interval term)
        attacks-intv (map to-interval attacks)]
    (map new-terminal-from-interval
         (loop [targets (list term-intv)
                [attack & attacks] attacks-intv]
           (if attack
             (recur (mapcat #(intv/split-if-intersecting % attack) targets) attacks)
             targets)))))

(defn is-epsilon
  [terminal]
  (instance? EpsilonTerminal terminal))

(defn is-terminal?
  [terminal]
  (satisfies?  Terminal terminal))
