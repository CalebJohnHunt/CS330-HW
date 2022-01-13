#lang plait

; Helper Functions
(pow : (Number Number -> Number))
(define (pow base ex)
  (if (< ex 1)
    1 ; If exponent < 1, return 1
    (* base (pow base (- ex 1))) ; if ex >= 1, then recursively multiply
  )
)

; End of Helper Functins

