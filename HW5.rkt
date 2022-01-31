#lang plait
(print-only-errors #t)

(define-type Op
  (addO)
  (subO)
  (mulO))

(define-type Expr
  (numE [value : Number])
  (binE [op : Op]
        [lhs : Expr]
        [rhs : Expr])
  (if0E [tst : Expr]
        [thn : Expr]
        [els : Expr]))

(parse-op : (S-Exp -> Op))
(define (parse-op op)
  (cond
    [(s-exp-match? `+ op)
      (addO)]
    [(s-exp-match? `- op)
      (subO)]
    [(s-exp-match? `* op)
      (mulO)]
    [else
      (error 'parse-op (string-append "Unkonwn op: " (to-string op)))]
  )
)

(parse : (S-Exp -> Expr))
(define (parse s)
  (cond
    [(s-exp-match? `NUMBER s) ; numE
      (numE (s-exp->number s))]
    [(s-exp-match? `(SYMBOL ANY ANY) s) 
      (let ([ss (s-exp->list s)]) ; binE
          (binE
            (parse-op (first  ss)) ; op
            (parse    (second ss)) ; lhs
            (parse    (third  ss)) ; rhs
          )
        )]
    [(s-exp-match? `(if0 ANY ANY ANY) s)
      (let ([ss (s-exp->list s)])
        (if0E
          (parse (second  ss)) ; tst
          (parse (third   ss)) ; thn
          (parse (fourth  ss)) ; els
        )
      )]
    [else (error 'parse (string-append "Bad input to parse:\n" (to-string s)))]
  )
)

(begin
  (test (parse `10) (numE 10))
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
)

(interp : (Expr -> Number))
(define (interp e)
  (type-case Expr e
    [(numE value)
      value]
    [(binE op lhs rhs)
      (type-case Op op
        [(addO)
          (+
            (interp lhs)
            (interp rhs)
          )]
        [(subO)
          (-
            (interp lhs)
            (interp rhs)
          )]
        [(mulO)
          (*
            (interp lhs)
            (interp rhs)
          )]
      )]
    [(if0E tst thn els)
      (if (zero? (interp tst))
          (interp thn)
          (interp els)
      )]
  )
)

(test (interp (parse `5)) 5)
(test (interp (parse `(+ 2 3))) 5)
(test (interp (parse `(- 3 2))) 1)
(test (interp (parse `(* 3 2))) 6)
(test (interp (parse `(* (- 3 1) (+ 2 4)))) 12)
(test (interp (parse `(if0 0 1 2))) 1)
(test (interp (parse `(if0 1 1 2))) 2)
(test (interp (parse `(if0 (- 2 2) 1 2))) 1)
(test (interp (parse `(if0 (- 2 1) 1 2))) 2)
(test (interp (parse `(* (if0 (- 2 1) 1 2) 4))) 8)

