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

(sum-coins : (Number Number Number Number -> Number))
(define (sum-coins pennies nickels dimes quarters)
	(+
		(+ pennies (* nickels  5))
		(+ (* dimes    10) (* quarters 25))
	)
)

(define pi : Number 3.1415926535897)

(area-cylinder : (Number Number -> Number))
(define (area-cylinder base-radius height)
	(* 2
		(+
			(* pi (* base-radius height))
			(* pi (* base-radius base-radius))

		)
	)
)

;   2(r)(pi)(h) + 2(r+w)(pi)(h) + 2((pi)(r+w)^2-(pi)(r)^2)
; = 2[(r)(pi)(h) + (r+w)(pi)(h) + ((pi)(r+w)^2-(pi)(r^2))]
; = 2pi[(r)(h) + (r+w)(h) + ((r+w)^2 - (r)^2)]
; = 2pi[h(r+r+w)+(r+w)^2-(r)^2]

(area-pipe : (Number Number Number -> Number))
(define (area-pipe inner-radius wall-thickness height)
	(* 2
		(* pi
			(+
				(*
					height
					(+ (+ inner-radius inner-radius) wall-thickness)
				)
				(-
					(pow (+ inner-radius wall-thickness) 2)
					(pow inner-radius 2)
				)
			)
		)
	)
)

; Look at everything about 480. Tax it.
; Look at everything above 240. Don't double count stuff about 480. Tax it.
; Ignore below 240
; Sum the taxes

(tax : (Number -> Number))
(define (tax gross-pay)
	(+
		; Highest tax bracket
		(if (> gross-pay 480)
			(* (- gross-pay 480) 0.28)
			0)
		; Middle tax bracket
		(if (> gross-pay 240)
			(* (- 
					(min 480 gross-pay)
					240
			 	) 0.15)
			0
		)
	)
)

(net-pay : (Number Number -> Number))
(define (net-pay hours-worked hourly-wage)
	(-
		(* hours-worked hourly-wage)
		(tax (* hours-worked hourly-wage))
	)
)

; Determinate: b^2 - 4ac
; +: 2 solutions, 0: 1 solution, -: no solutions
(what-kind : (Number Number Number -> Symbol))
(define (what-kind a b c)
	(let ([determinate
			(-
				(* b b)
				(* 4 (* a c))
			)])
	
		(if (= a 0)
			'degenerate
			(cond
				[(> determinate 0) 'two]
				[(= determinate 0) 'one]
				[else 'none]
			)
		)
	)
)

(define-type Time
	(hms [hours : Number] [minutes : Number] [seconds : Number]))

(time-diff : (Time Time -> Number))
(define (time-diff t1 t2)
	(-
		(+ (* (hms-hours t2) 3600) (+ (* (hms-minutes t2) 60) (hms-seconds t2)))
		(+ (* (hms-hours t1) 3600) (+ (* (hms-minutes t1) 60) (hms-seconds t1)))
	)
)

(define-type Position (position [x : Number] [y : Number]))
(define-type Shape
  (circle [center : Position]
          [radius : Number])
  (square [upper-left : Position]
          [side-length : Number])
  (rectangle [upper-left : Position]
             [width : Number]
             [height : Number]))

(area : (Shape -> Number))
(define (area the-shape)
	(type-case Shape the-shape
		[(circle center radius)
			(* pi (* radius radius))]
		[(square upper-left side-length)
			(* side-length side-length)]
		[(rectangle upper-left width height)
			(* width height)]
	)
)

(translate-shape : (Shape Number -> Shape))
(define (translate-shape shape delta)
	(type-case Shape shape
		[(circle center radius)
			(circle (position (+ (position-x center) delta) (position-y center)) radius)]
		[(square upper-left side-length)
			(square (position (+ (position-x upper-left) delta) (position-y upper-left)) side-length)]
		[(rectangle upper-left width height)
			(rectangle (position (+ (position-x upper-left) delta) (position-y upper-left)) width height)]
	)
)

; I'm assuming that the positive-y direction is up (like a regular graph in algebra class)
(in-shape? : (Shape Position -> Boolean))
(define (in-shape? s p)
	(type-case Shape s
		[(circle c r)
			(< 
				(+ 
					(pow (- (position-x p) (position-x c)) 2)
					(pow (- (position-y p) (position-y c)) 2)
				)
				(* r r)
			)]
		[(square ul sl)
			(and 
				(and 
					(< (position-x p) (+ (position-x ul) sl))
					(> (position-x p) (position-x ul))
				) 
				(and
					(< (position-y p) (position-y ul))
					(> (position-y p) (- (position-y ul) sl))
				)
			)]
		[(rectangle ul w h)
			(and 
				(and 
					(< (position-x p) (+ (position-x ul) w))
					(> (position-x p) (position-x ul))
				) 
				(and
					(< (position-y p) (position-y ul))
					(> (position-y p) (- (position-y ul) h))
				)
			)
		]
	)
)

"Tests:"

; Tests

; sum-coins
(test (sum-coins 3 7 5 12) 388)

; area-cylinder
(test (area-cylinder 329 7.92) 696470.2311256167)

; area-pipe
(test (area-pipe 16 80 76) 109779.81368703848)

; tax
(test (tax 600) 69.60)

; net-pay
(test (net-pay 6 100) (- (* 6 100) 69.6))

; what-kind
(test (what-kind 0 1 2)  'degenerate)
(test (what-kind 1 0 -1) 'two)
(test (what-kind 1 4 4)  'one)
(test (what-kind 1 0 1)  'none)

; time-diff
(test (time-diff (hms 1 2 3) (hms 2 3 4)) 3661)
(test (time-diff (hms 0 7 0) (hms 16 49 0)) (+ (* 16 3600) (* 42 60))) 

; area
(test (area (circle (position 0 0) 16)) (* 256 pi))
(test (area (square (position 0 0) 9/2)) (* 9/2 9/2))
(test (area (rectangle (position 0 0) 92 17.3)) (* 92 17.3))

; translate-shape
(define circle-in  (circle (position 10 25) 30 ))
(define circle-out (circle (position 20 25) 30 ))
(define square-in  (square (position 17 25) 30 ))
(define square-out (square (position 27 25) 30 ))
(define rectangle-in  (rectangle (position -12 300) 80 1 ))
(define rectangle-out (rectangle (position -2  300) 80 1 ))
(test (translate-shape circle-in 10) circle-out)
(test (translate-shape square-in 10) square-out)
(test (translate-shape rectangle-in 10) rectangle-out)

; in-shape?
(define p_in  (position -7 3))
(define p_out (position 31 -60))
(define circ  (circle    (position -2 5) 15))
(define squa  (square    (position -10 5) 5))
(define rect  (rectangle (position -12 12) 8 16))
(test (in-shape? circ p_in)  #t)
(test (in-shape? circ p_out) #f)
(test (in-shape? squa p_in)  #t)
(test (in-shape? squa p_in)  #t)
(test (in-shape? rect p_out) #f)
(test (in-shape? rect p_out) #f)

