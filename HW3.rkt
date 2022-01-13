#lang plait

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

