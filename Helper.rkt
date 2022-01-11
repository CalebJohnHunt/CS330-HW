#lang plait

; Helper Functions
(pow : (Number Number -> Number))
(define (pow base ex)
  (if (< ex 1)
    1 ; If exponent < 1, return 1
    (* base (pow base (- ex 1))) ; if ex >= 1, then recursively multiply
  )
)

(applyto : (('a -> 'b) (Listof 'a) -> (Listof 'b)))
(define (applyto func aList)
  (type-case (Listof 'a) aList
    [empty empty]
    [(cons x rest-x)
      (cons (func x) (applyto func rest-x))
    ]
  )
)
; End of Helper Functins

