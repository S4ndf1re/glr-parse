(ns glr-parser.graph.interval)


(defn is-interval-intersecting
  "Test if the attacker is either partially, or fully intersected with the target.
  Expectes both to be a simple list, start to end, end inclusive"
  [target attacker]
  (not (or (< (second target) (first attacker)) ;; either completely to the left
           (> (first target) (second attacker)) ;; or completely to the right
           (and (>= (first target) (first attacker)) ;; or subset or equal of the attacker
                (<= (second target) (second attacker))))))

(defn split-if-intersecting
  "Split target, when intersecting with attacker, resulting in up to 3 segments"
  [target attacker]
  (if (is-interval-intersecting target attacker)
    ;; Since the attacker is intersecting the target, determine splits
    (cond
      ;; attacker is on right of target, creating two intervals
      (and (< (first target) (first attacker))
           (<= (second target)
               (second attacker)))
      (list [(first target) (- (first attacker) 1)] attacker)
      ;; attacker is on left of target, creating two intervals
      (and (> (second target)
              (second attacker))
           (>= (first target)
               (first attacker)))
      (list attacker [(+ (second attacker) 1) (second target)])
      ;; Attacker splits target in the middle, creating 3 new intervals
      :else
      (list [(first target) (- (first attacker) 1)] attacker [(+ (second attacker) 1) (second target)]))
    ;; No splits needed, just return the target as is. Wrap in list to comply to function signature
    (list target)))
