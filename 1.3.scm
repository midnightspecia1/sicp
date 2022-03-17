;1.3
(define (sum-of-squares-two-largest a b c)
(cond ((< c a b) sum-of-squares a b)
      ((< a b c) sum-of-squares b c)
      ((< b a c) sum-of-squares a c)))