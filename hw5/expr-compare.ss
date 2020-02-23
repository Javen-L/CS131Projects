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
		; if only one list is empty, then they must not match
		[(or (null? x) (null? y))
			(list 'if '% x y)
		]
		; if the lists have different lenghts, then they must not match
		[(not (= (length x) (length y)))
			(list 'if '% x y)
		]
		; if both of them are lists, take first element, then parse rest
		[else
			(let ((x1 (car x)) (y1 (car y)))
				(cond
					;both start with quote
					[(and (equal? x1 'quote) (equal? y1 'quote))
						(if (equal? (cadr x) (cadr y)) (append (list x1 (cadr x)) (expr-compare (cddr x) (cddr y))) (cons (list 'if '% (list x1 (cadr x)) (list y1 (cadr y))) (expr-compare (cddr x) (cddr y))))
					]
					;one starts with quote
					[(equal? x1 'quote) (cons (list 'if '% (list x1 (cadr x)) y1) (expr-compare (cddr x) (cdr y)))]
					[(equal? y1 'quote) (cons (list 'if '% x1 (list y1 (cadr y))) (expr-compare (cdr x) (cddr y)))]
					;check for lambda expression (replace lambda with λ)
					;check for if expression
					[(and (equal? x1 'if) (= (length x) 4))
						(cond
							[(and (equal? y1 'if) (= (length y) 4)) (list 'if (expr-compare (cadr x) (cadr y)) (expr-compare (caddr x) (caddr y)) (expr-compare (cadddr x) (cadddr y)))]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? x1 'if) (= (length x) 3))
						(cond
							[(and (equal? y1 'if) (= (length y) 3)) (list 'if (expr-compare (cadr x) (cadr y)) (expr-compare (caddr x) (caddr y)))]
							[else (list 'if '% x y)]
						)
					]
					[(equal? y1 'if)
						(list 'if '% x y)
					]
					;check for list
					[(and (list? x1) (list? y1)) (cons (expr-compare x1 y1) (expr-compare (cdr x) (cdr y)))]
					[(or (list? x1) (list? y1)) (cons (list 'if '% x1 y1) (expr-compare (cdr x) (cdr y)))]
					;else
					[else (if (equal? x1 y1) (cons x1 (expr-compare (cdr x) (cdr y))) (cons (list 'if '% x1 y1) (expr-compare (cdr x) (cdr y))))]
				)
			)		
		]
	)
)
(expr-compare 12 12)
(expr-compare 12 20)
(expr-compare #t #t)
(expr-compare #f #f)
(expr-compare #t #f)
(expr-compare #f #t)
(expr-compare 'a '(cons a b))
(expr-compare '(cons a b) '(cons a b))
(expr-compare '(cons a lambda) '(cons a λ))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(cons a b) '(list a b))
(expr-compare '(list) '(list a))
(expr-compare ''(a b) ''(a c))
(expr-compare '(quote (a b)) '(quote (a c)))
(expr-compare '(quoth (a b)) '(quoth (a c)))
(expr-compare '(if x y z) '(if x z z))
(expr-compare '(if x y z) '(g x y z))
