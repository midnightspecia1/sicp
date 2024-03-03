(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

;Special keyword can't be expanded: (else else-clause)
;it seems like such things are forbidden by the interpreter
