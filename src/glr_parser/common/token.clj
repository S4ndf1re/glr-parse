(ns glr-parser.common.token)

(defn new-token
  ([ident value filename start end]
   (new-token ident value value filename start end))
  ([ident value transformed filename start end]
   {:ident ident
    :filename filename
    :start start
    :end end
    :raw-data value
    :data transformed}))

(defn filename
  "Get the tokens defining file"
  [tok]
  (:filename tok))

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

(defn location
  "Get the full location from the token"
  [tok]
  {:start (start tok)
   :end (end tok)
   :filename (filename tok)})

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
