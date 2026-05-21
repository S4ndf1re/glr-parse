(ns glr-parser.parser.precedence
  "Precedences are represented by numbers. Lower numbers mean lower precedence, higher mean higher precedence")

(def Precedence
  :int)

(def PrecedenceOrNil
  [:or
   :int
   :nil])

(def Associativity
  [:enum :left :right :none])

(def PrecedenceAssociativityTuple
  [:cat
   #'Precedence
   [:? Associativity]])

(def ObligatoryPrecedenceAssociativityTuple
  [:tuple
   #'Precedence
   #'Associativity])

(def ^:private associativity-as-numbers
  {:left 0
   :right 1
   :none 2})

(defn to-tuple
  [val]
  (let [[precedence associativity] val
        val [precedence (associativity-as-numbers associativity)]]
    val))

(defn cmp
  [left right]
  (let [left (to-tuple left)
        right (to-tuple right)]
    (compare left right)))
