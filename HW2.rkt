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

(find-temperate : ((Listof Number) -> (Optionof Number)))
(define (find-temperate nums)
	(type-case (Listof Number) nums
		[empty
			(none)]
		[(cons temp rest-temps)
			(if (and (>= temp 5) (<= temp 95))
				(some temp)
				(find-temperate rest-temps)
			)
		]
	)
)

#|
(test (find-temperate (list 4 30 50))                (some 30))
(test (find-temperate (list 0 0 0))                  (none))
(test (find-temperate (list))                        (none))
(test (find-temperate (list 4 1000 96 96 95))        (some 95))
(test (find-temperate (list -1 3 4 96 97 98 99 100)) (none))
|#

(check-temperate : ((Listof Number) -> Boolean))
(define (check-temperate temps)
	(type-case (Listof Number) temps
		[empty
			#true]
		[(cons temp rest-temps)
			(if (or (< temp 5) (> temp 95))
				#false
				(check-temperate rest-temps)
			)
		]
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
	(type-case (Listof Number) temps
		[empty
			#true]
		[(cons temp rest-temps)
			(if (or (< temp low) (> temp high))
				#false
				(check-temps rest-temps low high)
			)
		]
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
	(convert-helper nums 0)
)

(convert-helper : ((Listof Number) Number -> Number))
(define (convert-helper nums expo)
	(type-case (Listof Number) nums
	[empty 0]
	[(cons num rest-nums)
		(+ (* num (pow 10 expo)) (convert-helper rest-nums (+ expo 1)))]
	)
)

#|
(test (convert '(1 2 3)) 321)
(test (convert '()) 0)
(test (convert '( 0 0 0 2 4 0 8 0 )) 08042000)
(test (convert '(0 0 1 0 0)) 100)
|#

(define-type (RunningTotal)
	(rt [total : Number] [count : Number])
)

(average-price : ((Listof Number) -> Number))
(define (average-price prices)
	(type-case RunningTotal (average-price-helper prices 0)
		[(rt total count)
			(if (= count 0)
				0
				(/ total count)
			)
		]
	)
)

(average-price-helper : ((Listof Number) Number -> RunningTotal))
(define (average-price-helper prices count)
	(type-case (Listof Number) prices
		[empty (rt 0 count)]
		[(cons price rest-prices)
			(type-case RunningTotal (average-price-helper rest-prices count)
				[(rt total count)
					(rt (+ price total) (+ count 1))
				]
			)
		]
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
	(applyto f-to-c fs) ; See helper functions. `applyto` applies the function to the list
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
	(type-case (Listof Number) prices
		[empty empty]
		[(cons price rest-prices)
			(if (> price high)
					(eliminate-exp high rest-prices)
					(cons price (eliminate-exp high rest-prices))
			)
		]
	)
)

#|
(test (eliminate-exp 5 '(1 2 3 4)) '(1 2 3 4))
(test (eliminate-exp 5 '(10)     ) '()       )
(test (eliminate-exp 15.51 '(15 15.5 15.51 15.52 16 100 -1)) '(15 15.5 15.51 -1))
(test (eliminate-exp 0 (list)) (list))
|#

(suffixes : ((Listof 'a) -> (Listof (Listof 'a))))
(define (suffixes L)
	(type-case (Listof 'a) L
		[empty 
			(list empty)]
		[(cons L1 rest-L)
			;(cons L rest-L)
			(cons L (suffixes rest-L))
		]
	)
)


#|
(test (suffixes (cons 'a (cons 'b (cons 'c (cons 'd empty))))) '((a b c d) (b c d) (c d) (d) ()))
(test (suffixes '()) '(()))
(test (suffixes (list '(a) '(b))) '(((a) (b)) ((b)) ()))
|#

(define-type Pedigree
	(person [name : String]
					[birth-year : Number]
    			[eye-color : Symbol]
    			[father : Pedigree]
    			[mother : Pedigree])
	(unknown)
)

(count-persons : (Pedigree -> Number))
(define (count-persons ped)
	(type-case Pedigree ped
		[(unknown) 0]
		[(person n b e father mother)
			(+ 1
				(+
					(count-persons father)
					(count-persons mother)
				)
			)
		]
	)
)


(define dad (person "John"  1968 'brown (unknown) (unknown)))
(define mom (person "Becca" 1970 'hazel (person "James" 1930 'hazel (unknown) (unknown)) (unknown)))
(define me  (person "Caleb" 2000 'brown dad mom))

#|
(test (count-persons dad) 1)
(test (count-persons mom) 2)
(test (count-persons me ) 4)
(test (count-persons (unknown)) 0)
|#

(define current-year 2022)
(sum-pedigree-ages : (Pedigree -> Number))
(define (sum-pedigree-ages ped)
	(type-case Pedigree ped
		[(unknown) 0]
		[(person n birth e father mother)
			(+ (- current-year birth)
				(+
					(sum-pedigree-ages father)
					(sum-pedigree-ages mother)
				)
			)
		]
	)
)

#|
(test (sum-pedigree-ages dad) 54)
(test (sum-pedigree-ages mom) (+ 52 92))
|#

; I really wish I did this in O(n), but O(2n) is technically the same
(average-age : (Pedigree -> Number))
(define (average-age ped)
	(type-case Pedigree ped
		[(unknown) 0]
		[(person n b e f m)
			(/ (sum-pedigree-ages ped) (count-persons ped))
		]
	)
)

#|
(test (average-age dad) 54)
(test (average-age mom) 72)
(test (average-age me ) 55)
|#

(eye-colors : (Pedigree -> (Listof Symbol)))
(define (eye-colors ped)
	(type-case Pedigree ped
		[(unknown) '()]
		[(person n b eye father mother)
			(append (list eye) (append (eye-colors father) (eye-colors mother)))
		]
	)
)

#|
(test (eye-colors me)  '(brown brown hazel hazel))
(test (eye-colors dad) '(brown))
(test (eye-colors mom) '(hazel hazel))
(test (eye-colors (unknown)) '())
|#

