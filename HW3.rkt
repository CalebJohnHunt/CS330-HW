#lang plait

; Begin Helper Functions
(pow : (Number Number -> Number))
(define (pow base ex)
	(if (< ex 1)
		1
		(* base (pow base (- ex 1)))
	)
)
; End Helper Functions

(compose-func : (('b -> 'c) ('a -> 'b) -> ('a -> 'c)))
(define (compose-func after before)
	(lambda (a) (after (before a)))
)

#;
(local [(define (add5 x) (+ x 5))
				(define (sub4 x) (- x 4))]
	(test ((compose-func add5 sub4) 10) 11)
)

(flatten : ((Listof (Listof 'a)) -> (Listof 'a)))
(define (flatten loloa)
	(type-case (Listof (Listof 'a)) loloa
		[empty empty]
		[(cons loa rest-loa)
			(append loa (flatten rest-loa))
		]
	)
)

#|
(test (flatten (list (list 1 2) (list 3 4 5) (list 6))) '(1 2 3 4 5 6))
(test (flatten (list empty)) empty)
|#

(flatten-foldr : ((Listof (Listof 'a)) -> (Listof 'a)))
(define (flatten-foldr loloa)
	(foldr append empty loloa)
)

#|
(test (flatten-foldr (list (list 1 2) (list 3 4 5) (list 6))) '(1 2 3 4 5 6))
(test (flatten-foldr (list empty)) empty)
|#

(bucket : ((Listof Number) -> (Listof (Listof Number))))
(define (bucket lon)
	(foldr bucket-helper empty lon)
)

(bucket-helper : (Number (Listof (Listof Number)) -> (Listof (Listof Number))))
(define (bucket-helper num buckets)
	(type-case (Listof (Listof Number)) buckets
		[empty (list (list num))] ; First bucket!
		[(cons cur-bucket rest-buckets) ; We already have bucket(s)
			(type-case (Listof Number) cur-bucket ; Multiple ways to do this: We could use (define bucket-num (first cur-bucket)) instead
				[empty empty] ; We never make lists of empty, so this shouldn't be possible.
				[(cons bucket-num _rest-num) ; Just getting the first number in the bucket so we know what type of bucket it is
					(if (= num bucket-num) ; Should this new number join the previous bucket or make a new one?
						(cons (cons num cur-bucket) rest-buckets) ; Join new number to old bucket
						(cons (list num) buckets) ; Create new bucket for the new number
					)
				]
			)
		]
	)
)

#|
(test (bucket '(1 1 2 2 2 3 1 1 1 2 3 3)) '((1 1) (2 2 2) (3) (1 1 1) (2) (3 3)))
(test (bucket empty) empty)
(test (bucket '(1 1 1 1 2 1 1 1 1)) '((1 1 1 1) (2) (1 1 1 1)))
|#

(define-type Pedigree
  (person [name : String]
          [birth-year : Number]
          [eye-color : Symbol]
          [father : Pedigree]
          [mother : Pedigree])
  (unknown)
)

(tree-map : ((String -> String) Pedigree -> Pedigree))
(define (tree-map f ped)
	(type-case Pedigree ped
		[(unknown) (unknown)]
		[(person name _b _e father mother)
			(person
				(f name)
				_b
				_e
				(tree-map f father)
				(tree-map f mother)
			)
		]
	)
)

(add-last-name : (Pedigree String -> Pedigree))
(define (add-last-name ped last-name)
	(local [(define (space-append s1) (string-append (string-append s1 " ") last-name))]
		(tree-map space-append ped)
	)
)

#|
(define dad (person "John"  1968 'brown (unknown) (unknown)))
(define mom (person "Becca" 1970 'hazel (person "James" 1930 'hazel (unknown) (unknown)) (unknown)))
(define me  (person "Caleb" 2000 'brown dad mom))

(test (add-last-name dad "Johnson") (person "John Johnson" 1968 'brown (unknown) (unknown)))
(test (add-last-name mom "Carlson") (person "Becca Carlson" 1970 'hazel (person "James Carlson" 1930 'hazel (unknown) (unknown)) (unknown)))
(test (add-last-name me "Anderson") (person "Caleb Anderson" 2000 'brown (person "John Anderson" 1968 'brown (unknown) (unknown)) (person "Becca Anderson" 1970 'hazel (person "James Anderson" 1930 'hazel (unknown) (unknown)) (unknown))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Refactoring old functions ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-temperate : ((Listof Number) -> Boolean))
(define (check-temperate temps)
	(foldr 
		(lambda ([x : Number] [b : Boolean]) (and b (and (>= x 5) (<= x 95))))
		#true
		temps
	)
)

#|
(test (check-temperate (list 15 30 50))                #true)
(test (check-temperate (list 4 30 50))                 #false)
(test (check-temperate (list))                         #true)
(test (check-temperate (list 4 1000 96 96 95))         #false)
(test (check-temperate (list -1 3 4 96 97 98 99 100))  #false)
|#

(check-temps : ((Listof Number) Number Number -> Boolean))
(define (check-temps temps low high)
	(foldr 
		(lambda ([x : Number] [b : Boolean]) (and b (and (>= x low) (<= x high))))
		#true
		temps
	)
)

#|
(test (check-temps (list 15 30 50) 0 170)                #true)
(test (check-temps (list 4 30 50) 100 1000)              #false)
(test (check-temps (list) -100 500)                      #true)
(test (check-temps (list 4 1000 96 96 95) 40 90)         #false)
(test (check-temps (list -1 3 4 96 97 98 99 100) -1 100) #true)
|#

(convert : ((Listof Number) -> Number))
(define (convert nums)
	(foldr
		(lambda ([x : Number] [y : Number]) (+ (* y 10) x))
		0
		nums
	)
)

#|
(test (convert '(1 2 3)) 321)
(test (convert '()) 0)
(test (convert '( 0 0 0 2 4 0 8 0 )) 08042000)
(test (convert '(0 0 1 0 0)) 100)
|#

(average-price : ((Listof Number) -> Number))
(define (average-price prices)
	(let ([count (length prices)])
		(if
				(= count 0)
				0
				(/ (foldr + 0 prices) count)
		)
	)
)

#|
(test (average-price '(1)) 1)
(test (average-price '()) 0)
(test (average-price '(0 0 0)) 0)
(test (average-price '(1 3)) 2)
(test (average-price '(10 20 30 40 50 60)) 35)
(test (average-price '(12.2 15.8 -1 0.1 0.25 100 -100)) 3.907142857142856)
|#

(convertFC : ((Listof Number) -> (Listof Number)))
(define (convertFC fs)
	(map f-to-c fs)
)

; (F − 32) × 5/9 = C
(f-to-c : (Number -> Number))
(define (f-to-c f)
	(* (- f 32) 5/9)
)

#|
(test (convertFC '(-40 32 212)) '(-40 0 100))
(test (convertFC empty) empty)
(test (convertFC '(32 32 32 32)) '(0 0 0 0))
|#

(eliminate-exp : (Number (Listof Number) -> (Listof Number)))
(define (eliminate-exp high prices)
	(filter (lambda (num) (<= num high)) prices)
)

#|
(test (eliminate-exp 5 '(1 2 3 4)) '(1 2 3 4))
(test (eliminate-exp 5 '(10)     ) '()       )
(test (eliminate-exp 15.51 '(15 15.5 15.51 15.52 16 100 -1)) '(15 15.5 15.51 -1))
(test (eliminate-exp 0 (list)) (list))
|#

