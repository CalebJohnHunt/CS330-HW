#lang plait
; (print-only-errors #t)

(define-type-alias Value Number)
(define-type-alias Environment (Symbol -> Value))

(env-empty : Environment)
(define env-empty
  (λ (x) (error 'env-empty (string-append "Unbound identifier: " (symbol->string x))))
)

(env-extend : (Symbol Value Environment -> Environment))
(define (env-extend x v env)
    (lambda (z)
        (if (symbol=? x z)
            v
            (env z)
        )
    )
)

(env-lookup : (Symbol Environment -> Value))
(define (env-lookup x env)
    (env x)
)

(test/exn (env-lookup 'x env-empty) "")
(test/exn (env-lookup 'y (env-extend 'x 12 env-empty)) "")
(test (env-lookup 'x (env-extend 'x 12 env-empty)) 12)
(test (env-lookup 'x (env-extend 'y 13 (env-extend 'x 12 env-empty))) 12)
(test (env-lookup 'x (env-extend 'x 13 (env-extend 'x 12 env-empty))) 13)
