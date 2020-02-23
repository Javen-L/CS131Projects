#lang racket
;implement argument-replacer with revapp
(define (argument-replacer x r x3 out)
	(if (list? x3)
		(if (empty? x3)
			(reverse out)
			(let ((xh (car x3)))
				(cond
					[(equal? x xh) (argument-replacer x r (cdr x3) (cons r out))]
					[(list? xh) (argument-replacer x r (cdr x3) (cons (argument-replacer x r xh '() )))]
					[else (argument-replacer x r (cdr x3) (cons xh out))]
				)
			)
		)
		(cond
			[(equal? x x3) r]
			[else x3]
		)
	)
)
(define (argument-comparer x2 y2 x3 y3 out)
	(if (empty? x2)
		(list (reverse out) (expr-compare x3 y3))
		(let ((xh (car x2)) (yh (car y2)))
			(cond
				[(equal? xh yh) (argument-comparer (cdr x2) (cdr y2) x3 y3 (cons xh out))]
				[else
					(let ((r (string->symbol (string-append (symbol->string xh) "!" (symbol->string yh)))))
						(argument-comparer (cdr x2) (cdr y2) (argument-replacer xh r x3 '() ) (argument-replacer yh r y3 '() ) (cons r out))
					)
				]
			)
		)
	)
)
(define (lambda-parser x y)
	(let ((x1 (car x)) (y1 (car y)) (x2 (cadr x)) (y2 (cadr y)) (x3 (caddr x)) (y3 (caddr y)))
		(if (or (not (= (length x2) (length y2))) (and (list? x3) (not (list? y3))) (and (not (list? x3)) (list? y3)) (and (or (list? x3) (list? y3)) (not (= (length x3) (length y3)))))
			(list 'if '% x y)
			(if (or (equal? x1 'λ) (equal? y1 'λ))
				(cons 'λ (argument-comparer x2 y2 x3 y3 '()))
				(cons 'lambda (argument-comparer x2 y2 x3 y3 '()))
			)
		)
	)
)
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
		; if the lists have different lengths, then they must not match
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
					[(and (equal? x1 'λ) (= (length x) 3))
						(cond
							[(and (or (equal? y1 'λ) (equal? y1 'lambda)) (= (length y) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? y1 'λ) (= (length y) 3))
						(cond
							[(and (or (equal? x1 'λ) (equal? x1 'lambda)) (= (length x) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? x1 'lambda) (= (length x) 3))
						(cond
							[(and (equal? y1 'lambda) (= (length y) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? y1 'lambda) (= (length y) 3))
						(list 'if '% x y)
					]
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
					[(and (equal? y1 'if) (= (length y) 3))
						(list 'if '% x y)
					]
					;check for list
					[(and (list? x1) (list? y1)) (cons (expr-compare x1 y1) (expr-compare (cdr x) (cdr y)))]
					[(or (list? x1) (list? y1)) (cons (list 'if '% x1 y1) (expr-compare (cdr x) (cdr y)))]
					;else
					[else (if (equal? x1 y1) (cons x1 (expr-compare (cdr x) (cdr y))) (cons (expr-compare x1 y1) (expr-compare (cdr x) (cdr y))))]
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
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
