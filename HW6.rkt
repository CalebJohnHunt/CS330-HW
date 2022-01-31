#lang plait
(print-only-errors #t)

(define-type Op
  (addO)
  (subO)
  (mulO))

(define-type Expr
  (numE [value : Number])
  (ideE [name : Symbol])
  (binE [op : Op]
        [lhs : Expr]
        [rhs : Expr])
  (if0E [tst : Expr]
        [thn : Expr]
        [els : Expr])
  (letE [binds : (Listof (Symbol * Expr))]
        [body : Expr]))

(parse-op : (S-Exp -> Op))
(define (parse-op s)
  (cond
    [(s-exp-match? `+ s)
     (addO)]
    [(s-exp-match? `- s)
     (subO)]
    [(s-exp-match? `* s)
     (mulO)]
    [else
     (error 'parse "unrecognized operator")]))

(parse-binding : (S-Exp -> (Symbol * Expr)))
(define (parse-binding s)
  (cond
    [(s-exp-match? `[SYMBOL ANY] s)
     (let ([ss (s-exp->list s)])
       (values (s-exp->symbol (list-ref ss 0))
               (parse (list-ref ss 1))))]
    [else
     (error 'parse "unrecognized binding")]))

(parse : (S-Exp -> Expr))
(define (parse s)
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (ideE (s-exp->symbol s))]
    [(s-exp-match? `(if0 ANY ANY ANY) s)
     (let ([ss (s-exp->list s)])
       (if0E (parse (list-ref ss 1))
             (parse (list-ref ss 2))
             (parse (list-ref ss 3))))]
    [(s-exp-match? `(let ([SYMBOL ANY] ...) ANY) s)
     (let ([ss (s-exp->list s)])
       (letE (map parse-binding (s-exp->list (list-ref ss 1)))
             (parse (list-ref ss 2))))]
    [(s-exp-match? `(ANY ANY ANY) s)
     (let ([ss (s-exp->list s)])
       (binE (parse-op (list-ref ss 0))
             (parse (list-ref ss 1))
             (parse (list-ref ss 2))))]
    [else
     (error 'parse "unrecognized expression")]))

; Parsing tests
(begin (display "parse tests\n")
  (test (parse `5) (numE 5))
  (test (parse `x) (ideE 'x))
  (test (parse `(+ 2 3)) (binE (addO)
                              (numE 2)
                              (numE 3)))
  (test (parse `(- 2 3)) (binE (subO)
                              (numE 2)
                              (numE 3)))
  (test (parse `(* 2 3)) (binE (mulO)
                              (numE 2)
                              (numE 3)))
  (test (parse `(* (+ 2 3) (- 4 2)))
        (binE (mulO)
              (binE (addO)
                    (numE 2)
                    (numE 3))
              (binE (subO)
                    (numE 4)
                    (numE 2))))

  (test (parse `(if0 (* 2 1)
              (- 3 2)
              4))
        (if0E (binE (mulO) (numE 2) (numE 1))
              (binE (subO) (numE 3) (numE 2))
              (numE 4)))
  (test (parse `(let ([x 5]
                      [y 10])
                  (+ x y)))
        (letE (list (values 'x (numE 5))
                    (values 'y (numE 10)))
              (binE (addO)
                    (ideE 'x)
                    (ideE 'y))))
)

; here is an example of how to pull apart a pair
; 1. associate the name b to the pair of 'x and the AST of `(+ 1 2)
(let ([b (values 'x (parse `(+ 1 2)))])
  ; 2. use local to allow you to use define values to name
  ;    the components y and e so y is associated with 'x and e with the AST of `(+ 1 2)
  (local [(define-values (y e) b)]
    ; 3. use y and e in the body of the local
    (symbol=? y 'x)))

(subst/bs : (Expr Symbol (Listof (Symbol * Expr)) -> (Listof (Symbol * Expr))))
(define (subst/bs what for bs)
  (type-case (Listof (Symbol * Expr)) bs
    [empty empty]
    [(cons binding rest-bindings)
      (local [(define-values (s be) binding)]
        (cons (values s (subst what for be)) (subst/bs what for rest-bindings)) ; Substitute the value in place of the symbol in each bound expression
      )
    ]
  )
)

(ormap : (('a -> Boolean) (Listof 'a) -> Boolean))
(define (ormap p xs)
  (type-case (Listof 'a) xs
    [empty
     #f]
    [(cons x rest-xs)
     (or (p x)
         (ormap p rest-xs))]))

; Checks if the first parameter matches the first element in a pair
; Helper for `binds?`
(equals-first-in-pair? : ('a ('a * 'b) -> Boolean))
(define (equals-first-in-pair? t p)
  (local [(define-values (a b) p)]
    (equal? t a)
  )
)

(binds? : ((Listof (Symbol * Expr)) Symbol -> Boolean))
(define (binds? bs for)
  (ormap
    (λ (p) (equals-first-in-pair? for p))
    bs
  )
)

(begin (display "binds? tests\n")
  (test (binds? (list (pair 'Hello (numE 10))) 'Hello) #t)
  (test (binds? (list (pair 'Hello (numE 10))) 'Hey)   #f)
  (test (binds? (list (pair 'Hello (numE 10))
                      (pair 'foo   (parse `(if0 0 0 0)))) 'Hello)   #t)
  (test (binds? (list (pair 'Hello (numE 10))
                      (pair 'foo   (parse `(if0 0 0 0)))) 'foo)   #t)
)

(subst : (Expr Symbol Expr -> Expr))
(define (subst what for in)
  (type-case Expr in
    [(numE v) (numE v)]
    [(ideE x)
      (if (symbol=? for x)
          what     ; Symbols match? Substitute
          (ideE x) ; Symbols don't match? Leave it alone
      )
    ]
    [(binE op lhs rhs)
      (binE
        op
        (subst what for lhs)
        (subst what for rhs)
      )
    ]
    [(if0E tst thn els)
      (if0E
        (subst what for tst)
        (subst what for thn)
        (subst what for els)
      )
    ]
    [(letE bindings body)
      (letE
        (subst/bs what for bindings) ; Substitute inside the bound expression (shadowing can't prevent this)
        (if (binds? bindings for)    ; Is the newly bound symbol already bound inside the let?
          body                       ; Then don't substitute inside the let body
          (subst what for body)      ; Otherwise feel free to substitute as normal
        )
      )
    ]
    ; [else
    ;   (error 'subst (string-append "unknown Expr variant:\n\t" (to-string in)))]
  )
)

(begin (display "subst tests\n")
  (test (subst (parse `4) 'x (parse `5))
        (parse `5))
  (test (subst (parse `4) 'x (parse `x))
        (parse `4))
  (test (subst (parse `4) 'x (parse `y))
        (parse `y))
  (test (subst (parse `4) 'x (parse `(+ x y)))
        (parse `(+ 4 y)))
  (test (subst (parse `4) 'x (parse `(* z y)))
        (parse `(* z y)))
  (test (subst (parse `4) 'x (parse `(if0 x x y)))
        (parse `(if0 4 4 y)))
  (test (subst (parse `4) 'x (parse `(let ([y 5]) y)))
        (parse `(let ([y 5]) y)))
  (test (subst (parse `4) 'x (parse `(let ([y x]) y)))
        (parse `(let ([y 4]) y)))
  (test (subst (parse `4) 'x (parse `(let ([y x]) (+ y x))))
        (parse `(let ([y 4]) (+ y 4))))
  (test (subst (parse `4) 'x (parse `(let ([x 10]) x)))
        (parse `(let ([x 10]) x)))
  (test (subst (parse `4) 'x (parse `(let ([x x]) x)))
        (parse `(let ([x 4]) x)))
  (test (subst (parse `4) 'x (parse `(let ([x 10]
                                          [y 12])
                                      (+ x y))))
        (parse `(let ([x 10]
                      [y 12])
                  (+ x y))))
  (test (subst (parse `4) 'x (parse `(let ([z 10]
                                          [y 12])
                                      (+ x y))))
        (parse `(let ([z 10]
                      [y 12])
                  (+ 4 y))))
)
