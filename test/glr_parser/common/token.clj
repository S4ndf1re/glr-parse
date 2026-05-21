(ns glr-parser.common.token)

(defn new-token
  ([ident value start end]
   (new-token ident value value start end))
  ([ident value transformed start end]
   {:ident ident
    :start start
    :end end
    :raw-data value
    :data transformed}))

(defn range
  "Get the start-end range in the form [start, end) for a token"
  [tok]
  (list (:start tok) (:end tok)))

(defn start
  "Get the start-end range in the form [start, end) for a token"
  [tok]
  (:start tok))

(defn end
  "Get the start-end range in the form [start, end) for a token"
  [tok]
  (:end tok))

(defn ident
  "Get the token identifier as specified by the rule"
  [tok]
  (:ident tok))

(defn data
  "Get the value as a string. Note that the string conversion from vector of chars to string is performed in the new-token private function"
  [tok]
  (:data tok))

(defn raw-data
  "Get the value as a string. Note that the string conversion from vector of chars to string is performed in the new-token private function"
  [tok]
  (:raw-data tok))
