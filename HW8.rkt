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
       | (fun (x) expr)
       | (empty)
       | (cons expr expr)
       | (list-case e
           [(empty) expr]
           [(cons x y) expr])

|#

(define-type Op
  (addO)
  (subO)
  (mulO)
  (divO))

(define-type Expr
  (numE [value : Number])
  (ideE [name : Symbol])
  (binE [op : Op]
        [lhs : Expr]
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
        [body : Expr])
  (empE)
  (conE [fst : Expr]
        [rst : Expr])
  (l-cE [subject : Expr]
        [empty-result : Expr]
        [fst : Symbol]
        [rst : Symbol]
        [cons-result : Expr])
  )

(define (parse-op s)
  (cond
    [(s-exp-match? `+ s)
     (addO)]
    [(s-exp-match? `- s)
     (subO)]
    [(s-exp-match? `* s)
     (mulO)]
    [(s-exp-match? `/ s)
     (divO)]
    [else
     (error 'parse "bad operator")]))

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
    [(s-exp-match? `(empty) s)
      (empE)]
    [(s-exp-match? `(cons ANY ANY) s)
      (conE (parse (s-exp-ref `(cons • _) s))
            (parse (s-exp-ref `(cons _ •) s)))] ; This second one should be (empE) or another (conE Expr Expr)
    [(s-exp-match? `(list-case ANY [(empty) ANY] [(cons SYMBOL SYMBOL) ANY]) s)
      (l-cE (parse         (s-exp-ref `(list-case • [(empty) _] [(cons _ _) _]) s))
            (parse         (s-exp-ref `(list-case _ [(empty) •] [(cons _ _) _]) s))
            (s-exp->symbol (s-exp-ref `(list-case _ [(empty) _] [(cons • _) _]) s))
            (s-exp->symbol (s-exp-ref `(list-case _ [(empty) _] [(cons _ •) _]) s))
            (parse         (s-exp-ref `(list-case _ [(empty) _] [(cons _ _) •]) s)) )]
    [(s-exp-match? `(ANY ANY ANY) s)
     (binE (parse-op (s-exp-ref `(• _ _) s))
           (parse (s-exp-ref    `(_ • _) s))
           (parse (s-exp-ref    `(_ _ •) s)))]
    [else
     (error 'parse (string-append "not in language" (to-string s)))]))

(test (parse `5) (numE 5))
(test (parse `(+ 1 2))
      (binE (addO)
            (numE 1)
            (numE 2)))
(test (parse `(* 1 2))
      (binE (mulO)
            (numE 1)
            (numE 2)))
(test (parse `(/ 6 2))
      (binE (divO)
            (numE 6)
            (numE 2)))
(test (parse `(* 1 (+ 2 3)))
      (binE (mulO)
            (numE 1)
            (binE (addO)
                  (numE 2)
                  (numE 3))))
(test (parse `(if0 0 1 2))
      (if0E (numE 0)
            (numE 1)
            (numE 2)))

(test (parse `(let ([x 1]) x))
      (letE 'x (numE 1) (ideE 'x)))

(test (parse `(fun (x) x))
      (funE 'x (ideE 'x)))

(test (parse `(app 1 2))
      (appE (numE 1) (numE 2)))

(test (parse `(app (fun (x) x) 42))
      (appE (funE 'x (ideE 'x))
            (numE 42)))

(test (parse `(let ([f (fun (y) y)]) (app f 42)))
      (letE 'f
            (funE 'y (ideE 'y))
            (appE (ideE 'f) (numE 42))))

; list examples
; make these for *your* benefit
(test (parse `(empty))
      (empE))
(test (parse `(cons 1 (empty)))
      (conE (numE 1) (empE)))
(test (parse `(cons 2 (cons 3 (empty))))
      (conE (numE 2) (conE (numE 3) (empE))))
(test (parse `(cons (cons 4 (empty)) (empty)))
      (conE (conE (numE 4) (empE)) (empE)))
(test (parse `(cons (cons 4 (empty)) (cons hey (empty))))
      (conE (conE (numE 4) (empE)) (conE (ideE 'hey) (empE))))


(define-type Value
  (numV [value : Number])
  (funV [param : Symbol]
        [body : Expr]
        [env : Env])
  (empV)
  (conV [fst : Value]
        [rst : Value]))

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

(interp-op : (Op -> (Value Value -> Value)))
(define (interp-op op)
  (type-case Op op
    [(addO) (wrap-op '+ +)]
    [(subO) (wrap-op '- -)]
    [(mulO) (wrap-op '* *)]
    [(divO)
     (wrap-op
      '/
      (λ (numer denom)
        (if (zero? denom)
          (error 'divide "divide by zero XXX")
          (/ numer denom))))]))

; ENVIRONMENT-BASED INTERPRETATION

(define-type-alias Env (Hashof Symbol Value))

(env-empty : Env)
(define env-empty (hash empty))

(env-extend : (Symbol Value Env -> Env))
(define (env-extend x v env)
  (hash-set env x v))

(env-lookup : (Symbol Env -> Value))
(define (env-lookup x env)
  (type-case (Optionof Value) (hash-ref env x)
    [(some v)
     v]
    [(none)
     (error 'interp "unbound identifier")]))

(interp/env : (Expr Env -> Value))
(define (interp/env e env)
  (type-case Expr e
    [(numE n)
     (numV n)]
    [(ideE x)
     (env-lookup x env)]
    [(binE op   ; : Op
           lhs  ; : Expr
           rhs) ; : Expr
     ((interp-op op)
      (interp/env lhs env)
      (interp/env rhs env))]
    [(if0E tst thn els)
     (type-case Value (interp/env tst env)
       [(numV n)
        (if (zero? n)
          (interp/env thn env)
          (interp/env els env))]
       [else
        (error 'interp "not a number")])]
    [(letE x bound-e
           body-e)
     (let ([v-x (interp/env bound-e env)])
       (let ([new-env (env-extend x v-x env)])
         (interp/env body-e new-env)))]
    [(appE fun arg)
     (type-case Value (interp/env fun env)
       [(funV x body clo-env)
        (interp/env body (env-extend x (interp/env arg env) clo-env))]
       [else
        (error 'interp "not a function")])]
    [(funE x body)
     (funV x body env)]
    [(empE)
      (empV)]
    [(conE e0 e1)
      (let ([e1-eval (interp/env e1 env)])
        (type-case Value e1-eval
          [(empV) 
            (conV (interp/env e0 env) e1-eval)]
          [(conV fst rst)
            (conV (interp/env e0 env) e1-eval)]
          [else (error 'interp "not a list")]
        ))
    ]
    [(l-cE subject empty-result fst rst cons-result)
      (let ([subj-eval (interp/env subject env)])
        (type-case Value subj-eval
          [(empV)
            (interp/env empty-result env)]
          [(conV x xs)
            (let ([env1 (env-extend fst x env)])
              (let ([env2 (env-extend rst xs env1)])
                (interp/env cons-result env2)
              )
            )]
          [else (error 'interp "not a list")]
        )
      )
    ]
    ))

(define (interp* s)
  (interp/env (parse s) env-empty))

(test (interp* `5) (numV 5))
(test (interp* `(+ 1 2)) (numV 3))
(test (interp* `(* 5 2)) (numV 10))
(test (interp* `(* 5 (+ 2 3))) (numV 25))
(test (interp* `(/ 6 2)) (numV 3))
(test/exn (interp* `(/ 6 0)) "divide by zero XXX")
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

; application

; - can apply a lambda
(test (interp* `(app (fun (x) x) 42)) (numV 42))

; - can apply result of various expressions
(test (interp* `(app (if0 0
                       (fun (x) (+ x 1))
                       (fun (y) (- y 1)))
                     10))
      (numV 11))
(test (interp* `(app (let ([f (fun (x) (+ x 1))]) f)
                     10))
      (numV 11))
(test (interp* `(app (let ([f (fun (x) (+ x 1))]) f)
                     10))
      (numV 11))

; - throws error if not a closure
(test/exn (interp* `(app 1 2)) "not a function")

; closures

; - captures bindings
(test (interp* `(app (let ([x 12])
                       (fun (y) x))
                     10))
      (numV 12))

; lists

; - empty creates empty
(test (interp* `(empty)) (empV))

; - good cons creates cons
(test (interp* `(cons 1 (empty))) (conV (numV 1) (empV)))

; - bad cons throws error
(test/exn (interp* `(cons 1 2)) "not a list")

; - list-case recognizes empty
(test (interp* `(list-case (empty)
                  [(empty) 42]
                  [(cons fst rst) 35]))
      (numV 42))

; - list-case recognizes cons
(test (interp* `(list-case (cons 1 (empty))
                  [(empty) 42]
                  [(cons fst rst) 35]))
      (numV 35))

; - list-case works with other expressions
(test (interp* `(let ([x (empty)])
                  (list-case x
                    [(empty) 42]
                    [(cons fst rst) 35])) )
      (numV 42))

; - list-case cons binds identifiers
(test (interp* `(list-case (cons 35 (empty))
                  [(empty) 42]
                  [(cons fst rst) fst]))
      (numV 35))
(test (interp* `(list-case (cons 35 (empty))
                  [(empty) 42]
                  [(cons fst rst)
                   (+ fst
                      (list-case rst
                        [(empty) 10]
                        [(cons fst rst) 12]))]))
      (numV 45))

; - list-case throws error if not list
(test/exn (interp* `(list-case 35
                      [(empty) 42]
                      [(cons fst rst) fst]))
          "not a list")
