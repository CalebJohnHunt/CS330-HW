#lang plait

(define-type Lexeme
  (lex-left-paren)
  (lex-right-paren)
  (lex-false)
  (lex-symbol [x : Symbol])
  (lex-string [x : String])
  (lex-number [x : Number]))

(define (read-char str i)
  (if (< i (string-length str))
      (some (string-ref str i))
      (none)))

(define (non-whitespace str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (cond
       [(member c (string->list " \t\n"))
        (non-whitespace str (add1 i))]
       [else
        i])]
    [(none)
     i]))

(define (index-of i y xs)
  (type-case (Listof 'a) xs
    [empty
     (none)]
    [(cons x rest-xs)
     (if (equal? x y)
         (some i)
         (index-of (add1 i) y rest-xs))]))

(define (lfalse str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (if (equal? c #\f)
         (values (lex-false) (add1 i))
         (error 'lex (string-append "expected #\f but got " (to-string c))))]
    [(none)
     (error 'lex (string-append "expected #\f but got " "end of input"))]))

(define (char-digit c)
  (index-of 0 c (string->list "0123456789")))

(define (lnum n str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (type-case (Optionof Number) (char-digit c)
       [(some d)
        (lnum (+ (* 10 n) d) str (add1 i))]
       [(none)
        (values (lex-number n) i)])]
    [(none)
     (values (lex-number n) i)]))

(define (lstring cs str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (let ([i (add1 i)])
       (cond
         [(equal? c #\")
          (values (lex-string (list->string (reverse cs))) i)]
         [(equal? c #\\)
          (type-case (Optionof Char) (read-char str i)
            [(some c)
             (lstring (cons (cond
                              [(equal? c #\n) #\newline]
                              [(equal? c #\t) #\tab]
                              [(equal? c #\\) #\\]
                              [else (error 'parse "expected escape character")])
                            cs)
                      str (add1 i))]
            [(none)
             (error 'parse "expected escape character")])]
         [else
          (lstring (cons c cs) str i)]))]
    [(none)
     (error 'parse "expected end of string")]))

(define (symbol-start? c)
  (member c (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_?!@$%^&*")))

(define (symbol-continuation? c)
  (member c (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_?!@$%^&*0123456789")))

(define (lsymbol cs str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (if (symbol-continuation? c)
         (lsymbol (cons c cs) str (add1 i))
         (values (lex-symbol (string->symbol (list->string (reverse cs)))) i))]
    [(none)
     (values (lex-symbol (string->symbol (list->string (reverse cs)))) i)]))

(define (l str i)
  (type-case (Optionof Char) (read-char str i)
    [(some c)
     (local [(define-values (x i*)
               (cond
                 [(equal? c #\()
                  (values (lex-left-paren) (add1 i))]
                 [(equal? c #\))
                  (values (lex-right-paren) (add1 i))]
                 [(equal? c #\#)
                  (lfalse str (add1 i))]
                 [(equal? c #\")
                  (lstring empty str (add1 i))]
                 [(member c (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_?!@$%^&*"))
                  (lsymbol (list c) str (add1 i))]
                 [else
                  (type-case (Optionof Number) (char-digit c)
                    [(some d)
                     (lnum d str (add1 i))]
                    [(none)
                     (error 'lex (string-append "unexpected char " (to-string c)))])]))]
       (cons x (l str (non-whitespace str i*))))]
    [(none)
     empty]))

(lex : (String -> (Listof Lexeme)))
(define (lex str)
  (l str (non-whitespace str 0)))

; Parse an atom.
; Symbol, number, string, and false all get converted to S-Exp.
; Left-paren and right-paren (and anything else) crash
(parse-atom : (Lexeme -> S-Exp))
(define (parse-atom atom)
  (type-case Lexeme atom
    [(lex-symbol x) (symbol->s-exp x)]
    [(lex-number x) (number->s-exp x)]
    [(lex-string x) (string->s-exp x)]
    [(lex-false)    `#f]
    [else
      (error 'parse-atom (string-append "Unexpected atom: " (to-string atom)))
    ]
  )
)

; Parse inside parentheses
; Lexems should look like: "atom* (atom*)* atom* )"
; Notice there is no starting ( but there is an ending )
(parse-inside : ((Listof S-Exp) (Listof Lexeme) -> (S-Exp * (Listof Lexeme))))
(define (parse-inside ss ts)
  (type-case (Listof Lexeme) ts
    [empty ; Must've kept looking for the closing parenthesis but it didn't exist
      (error 'parse-inside "Missing right parenthesis to close expression")
    ]
    [(cons first-lex rest-lexes)
      (type-case Lexeme first-lex
        [(lex-right-paren) ; Closing paren: Flatten the S-Exps and return the unread lexemes
          (values `{,@ss} rest-lexes)
        ]
        [(lex-left-paren) ; Opening paren: Start parsing inside this new piece and add its S-Exp to our list
          (local [(define-values (SE remaining-l) (parse-inside empty rest-lexes))]
            (parse-inside (append ss (list SE)) remaining-l)
          )
        ]
        [else ; Atom: Add it to the list and keep parsing inside
          (parse-inside (append ss (list (parse-atom first-lex))) rest-lexes)
        ]
      )
    ]
  )
)

; Parse list of Lexemes into one big S-Expression
(parse : ((Listof Lexeme) -> S-Exp))
(define (parse ts)
  (type-case (Listof Lexeme) ts
    [empty `{}]
    [(cons first-lex rest-lexes)
      (type-case Lexeme first-lex
        [(lex-left-paren) ; Left paren: Parse inside. Crash if there's more after the closing paren
          (local [(define-values (SE remaining-l) (parse-inside empty rest-lexes))]
            (if (empty? remaining-l) ; Are there extra lexemes? Hope not!
              SE
              (error 'parse "Must only have a single section (it should be an atom or else put everything inside parentheses)"))
          )
        ]
        [(lex-right-paren) ; Right paren: Crash. No way to start with right paren
          (error 'parse "Cannot start with a right parenthesis")
        ]
        [else ; Atom: Parse it. Crash if there's more afterwards
          (if (empty? rest-lexes)
            (parse-atom first-lex)
            (error 'parse "Cannot have multiple lexemes unless inside parentheses (maybe put everything in parentheses?)")
          )
        ]
      )
    ]
  )
)

; (print-only-errors #t)

; (test/exn (parse (lex "(a))"))   "single section")
; (test/exn (parse (lex "(a)("))   "single section")
; (test/exn (parse (lex "(a)(b)")) "single section")
; (test/exn (parse (lex "(hello there 123 (world) #f #f (inside (inside (inside)))")) "Missing right parenthesis")
; (test/exn (parse (lex "hey there man")) "multiple lexemes")
; (test/exn (parse (lex ") 123")) "start with a right parenthesis")

; (test (parse (lex "")) `{})
; (test (parse (lex "(hello there 123 (world) #f #f (inside (inside (inside))))")) `{hello there 123 {world} #f #f {inside {inside {inside}}}})
; (test (parse (lex "single_atom")) `single_atom)