#lang plait
(print-only-errors #t)

#|

plait--

program ::= expr

expr ::= number
       | x
       | (+ expr expr)
       | (* expr expr)
       | (/ expr expr)
       | (if0 expr expr expr)
       | (let ([x expr]) expr)
       | (app expr expr)

|#

; ignore until the next XXX

(define-type Expr
  (numE [value : Number])
  (ideE [name : Symbol])
  (addE [lhs : Expr]
        [rhs : Expr])
  (if0E [tst : Expr]
        [thn : Expr]
        [els : Expr])
  (letE [name : Symbol]
        [bound-expr : Expr]
        [body-expr : Expr])
  (appE [function : Expr]
        [argument : Expr])
  (funE [param : Symbol]
        [body : Expr]))

(define (seref* pp ss fail)
  (type-case (Listof S-Exp) pp
    [empty
     (type-case (Listof S-Exp) ss
       [empty (none)]
       [(cons _₀ _₁) (fail)])]
    [(cons p pp)
     (type-case (Listof S-Exp) ss
       [empty (fail)]
       [(cons s ss)
        (type-case (Optionof S-Exp) (seref p s fail)
          [(some s) (some s)]
          [(none) (seref* pp ss fail)])])]))

(seref : (S-Exp S-Exp (-> 'a) -> (Optionof S-Exp)))
(define (seref p s fail)
  (cond
    [(s-exp-match? `• p)
     (some s)]
    [(s-exp-match? `_ p)
     (none)]
    [(equal? p s)
     (none)]
    [(s-exp-list? p)
     (if (s-exp-list? s)
       (let ([pp (s-exp->list p)]
             [ss (s-exp->list s)])
         (seref* pp ss fail))
       (fail))]
    [else
     (fail)]))

(define (s-exp-ref p s)
  (let ([fail (λ ()
                (error
                 's-exp-ref
                 (string-append
                  "bad reference "
                  (string-append
                   (to-string p)
                   (string-append
                    " in "
                    (to-string s))))))])
    (type-case (Optionof S-Exp) (seref p s fail)
    [(some s) s]
    [(none) (fail)])))

(define (parse s)
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (ideE (s-exp->symbol s))]
    [(s-exp-match? `(+ ANY ANY) s)
     (addE (parse (s-exp-ref    `(+ • _) s))
           (parse (s-exp-ref    `(+ _ •) s)))]
    [(s-exp-match? `(if0 ANY ANY ANY) s)
     (if0E (parse (s-exp-ref `(if0 • _ _) s))
           (parse (s-exp-ref `(if0 _ • _) s))
           (parse (s-exp-ref `(if0 _ _ •) s)))]
    [(s-exp-match? `(app ANY ANY) s)
     (appE (parse (s-exp-ref `(app • _) s))
           (parse (s-exp-ref `(app _ •) s)))]
    [(s-exp-match? `(let ([SYMBOL ANY]) ANY) s)
     (letE (s-exp->symbol (s-exp-ref `(let ([• _]) _) s))
           (parse (s-exp-ref         `(let ([_ •]) _) s))
           (parse (s-exp-ref         `(let ([_ _]) •) s)))]
    [(s-exp-match? `(fun (SYMBOL) ANY) s)
     (funE (s-exp->symbol (s-exp-ref `(fun (•) _) s))
           (parse         (s-exp-ref `(fun (_) •) s)))]
    [else
     (error 'parse (string-append "not in language" (to-string s)))]))

; XXX make sure you understand these definitions and their role!

(define-type-alias Location Number)

(define-type-alias Env (Hashof Symbol Location))

(env-empty : Env)
(define env-empty (hash empty))

(env-extend : (Symbol Location Env -> Env))
(define (env-extend x l env)
  (hash-set env x l))

(env-lookup : (Symbol Env -> Location))
(define (env-lookup x env)
  (type-case (Optionof Location) (hash-ref env x)
    [(some l)
     l]
    [(none)
     (error 'interp "unbound identifier")]))

(define-type-alias Store (Hashof Location Value))

(store-empty : Store)
(define store-empty (hash empty))

(store-extend : (Location Value Store -> Store))
(define (store-extend l v store)
  (hash-set store l v))

(store-lookup : (Location Store -> Value))
(define (store-lookup l store)
  (type-case (Optionof Value) (hash-ref store l)
    [(some v)
     v]
    [(none)
     (error 'interp (string-append "SEGMENTATION FAULT! Looking for: " (to-string l)))]))


(store-next-free : (Store -> Location))
(define (store-next-free sto)
  (length (hash-keys sto)))

; XXX end of section

(define-type Value
  (numV [value : Number])
  (funV [param : Symbol]
        [body : Expr]
        [env : Env]))

; INTERPRETATION

(wrap-op : (Symbol (Number Number -> Number) -> (Value Value -> Value)))
(define (wrap-op who f)
  (λ (v₀ v₁)
    (type-case Value v₀
      [(numV n₀)
       (type-case Value v₁
         [(numV n₁)
          (numV (f n₀ n₁))]
         [else
          (error who "not a number")])]
      [else
       (error who "not a number")])))

; XXX change this definition
(interp : (Expr Env Store -> (Value * Store)))
(define (interp exp env sto)
  (type-case Expr exp
    [(numE n)
     (values (numV n) sto)]
    [(ideE x)
     (values (store-lookup (env-lookup x env) sto) sto)]
    [(addE lhs rhs)
     (values ((wrap-op '+ +)
              (fst (interp lhs env sto))
              (fst (interp rhs env sto)))
              sto)]
    [(if0E tst thn els)
     (type-case Value (fst (interp tst env sto))
       [(numV n)
        (if (zero? n)
          (interp thn env sto)
          (interp els env sto))]
       [else
        (error 'interp "not a number")])]
    [(letE x bound-e
           body-e)
      (local [(define-values (v-x sto1) (interp bound-e env sto))]
        (let ([loc1 (store-next-free sto1)])
          (let ([sto2 (store-extend loc1 v-x sto1)])
            (let ([env1 (env-extend x loc1 env)])
              (interp body-e env1 sto2)
            )
          )
        )
      )
     #|
     (let ([v-x (interp bound-e env)])
      (let ([new-env (env-extend x v-x env)])
        (interp body-e new-env)
      )
     )|#
    ]
    [(appE fun arg)
      (local [(define-values (v-f sto1) (interp fun env sto))]
        (type-case Value v-f
          [(funV x body clo-env)
            (local [(define-values (v-x sto2) (interp arg env sto1))]
              (let ([loc1 (store-next-free sto2)])
                (let ([sto3 (store-extend loc1 v-x sto2)])
                  (let ([clo-env1 (env-extend x loc1 clo-env)])
                    (interp body clo-env1 sto3)
                  )
                )
              )
            )          
          ]
          [else
            (error 'interp "not a function")
          ]
        )
      )
    #|
     (type-case Value (interp fun env)
       [(funV x body clo-env)
        (let ([v-x (interp arg env)])
          (let ([new-env (env-extend x  v-x clo-env)])
            (interp body new-env)))]
       [else
        (error 'interp "not a function")])
    |#
    ]
    [(funE x body)
      (values (funV x body env) sto)
    ]
  )
)

(interp* : (S-Exp -> Value))
(define (interp* s)
  (fst (interp (parse s) env-empty store-empty)))

(test (interp* `5) (numV 5))
(test (interp* `(+ 1 2)) (numV 3))
(test (interp* `(let ([x 10])
                  (+ x x)))
      (numV 20))
(test (interp* `(let ([x (+ 5 5)]) 
                  (+ x x)))
      (numV 20))
(test (interp* `(let ([x (+ 5 5)])
                  (let ([y (+ 2 2)])
                    (+ x y))))
      (numV 14))

(test (interp* `(let ([x (+ 5 5)])
                  (let ([x x])
                    (+ x x))))
      (numV 20))
(test (interp* `(let ([x (+ 5 5)])
                  (let ([x (+ x 1)])
                    (+ x x))))
      (numV 22))
(test (interp* `(let ([x 10])
                  (+ (let ([x 12])
                       x)
                     x)))
      (numV 22))

(test (interp* `(if0 0 1 2))
      (numV 1))

(test (interp* `(app (fun (x) x) 42)) (numV 42))

(test (interp* `(app (if0 0
                       (fun (x) (+ x 1))
                       (fun (y) (+ y 2)))
                     10))
      (numV 11))
(test (interp* `(app (let ([f (fun (x) (+ x 1))]) f)
                     10))
      (numV 11))
(test (interp* `(app (let ([f (fun (x) (+ x 1))]) f)
                     10))
      (numV 11))

(test/exn (interp* `(app 1 2)) "not a function")

(test (interp* `(app (let ([x 12])
                       (fun (y) x))
                     10))
      (numV 12))
