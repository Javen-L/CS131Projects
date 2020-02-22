#lang racket
(define (expr-compare x y)
	(cond
		[(equal? x y) x]
		[(and (boolean? x) (boolean? y))
			(if x
				(if y #t '%)
				(if y '(not %) #f)
			)
		]
		; if one of them is not list - which means that not function
		[(or (not (list? x)) (not (list? y)))
			(list 'if '% x y)
		]
		[(or (null? x) (null? y))
			(list 'if '% x y)
		]
		; if both of them are lists, take first element, then parse rest
		[else
			(let ((x1 (car x)) (y1 (car y)))
				(cond
					;replace lambda with λ
					[(equal? x1 'lambda) (expr-compare (cons 'λ (cdr x)) y)]
					[(equal? y1 'lambda) (expr-compare x (cons 'λ (cdr y)))]
					;replace quote with '
					[(equal? x1 'quote) (expr-compare (quote (cdr x)) y)]
					[(equal? y1 'quote) (expr-compare x (quote (cdr y)))]
				)
			)		
		]
	)
)
