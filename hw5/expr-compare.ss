#lang racket
(provide (all-defined-out))
;implement argument-replacer with revapp
(define (argument-replacer-single x r x3 out)
	(if (list? x3)
		(if (empty? x3)
			(reverse out)
			(let ((xh (car x3)))
				(cond
					[(equal? x xh) (argument-replacer-single x r (cdr x3) (cons r out))]
					[(list? xh) (argument-replacer-single x r (cdr x3) (cons (argument-replacer-single x r xh '() ) out))]
					[else (argument-replacer-single x r (cdr x3) (cons xh out))]
				)
			)
		)
		(if (equal? x x3)
			r
			x3
		)
	)
)
(define (list-status x x3)
	; "single equal" --> single element that is equal
	; "single not equal" --> single element that is not equal
	; "list to check" --> expression without lambda or lambda without the variable defined
	; "list to not check" --> lambda expression with variable defined
	; "empty list" --> empty list, return
	; "replace single" --> replace a single element
	; "do not replace" --> do not replace single element
	(if (list? x3)
		(if (empty? x3)
			"empty list"
			(let ((xh (car x3)))
				(cond
					[(list? xh)
						(cond
							[(and (or (equal? (car xh) 'lambda) (equal? (car xh) 'λ)) (= (length xh) 3))
								(cond
									[(and (list? (cadr xh)) (member x (cadr xh)))
										"list to not check"
									]
									[(equal? (cadr xh) x) "list to not check" ]
									[else "list to check"]
								)
							]
							[else "list to check"]
						)
					]
					[(equal? x xh) "replace single"]
					[else "do not replace"]
				)
			)
		)
		(if (equal? x x3)
			"single equal"
			"single not equal"
		)
	)
)
(define (argument-replacer x y r x3 y3 outx outy)
	(let ((x-status (list-status x x3)) (y-status (list-status y y3)))
		(cond
			[(equal? x-status "empty list") (list (reverse outx) (reverse outy))]
			[(equal? x-status "single equal")
				(cond
					[(equal? y-status "single equal") (list r r)]
					[(equal? y-status "single not equal") (list r y)]
				)
			]
			[(equal? x-status "single not equal")
				(cond
					[(equal? y-status "single equal") (list x r)]
					[(equal? y-status "single not equal") (list x y)]
				)
			]
			[(equal? x-status "list to check")
				(cond
					[(equal? y-status "list to check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r (car x3) '() ) outx) (cons (argument-replacer-single y r (car y3) '() ) outy))]
					[(equal? y-status "list to not check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r (car x3) '() ) outx) (cons (car y3) outy))]
					[(equal? y-status "replace single") (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r (car x3) '() ) outx) (cons r outy))]
					[(equal? y-status "do not replace") (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r (car x3) '() ) outx) (cons (car y3) outy))]
				)
			]
			[(equal? x-status "list to not check")
				(cond
					[(equal? y-status "list to check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (argument-replacer-single y r (car y3) '() ) outy))]
					[(equal? y-status "list to not check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (car y3) outy))]
					[(equal? y-status "replace single") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons r outy))]
					[(equal? y-status "do not replace") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (car y3) outy))]
				)
			]
			[(equal? x-status "replace single")
				(cond
					[(equal? y-status "list to check") (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons (argument-replacer-single y r (car y3) '() ) outy))]
					[(equal? y-status "list to not check") (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons (car y3) outy))]
					[(equal? y-status "replace single") (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons r outy))]
					[(equal? y-status "do not replace") (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons (car y3) outy))]
				)
			]
			[(equal? x-status "do not replace")
				(cond
					[(equal? y-status "list to check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (argument-replacer-single y r (car y3) '() ) outy))]
					[(equal? y-status "list to not check") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (car y3) outy))]
					[(equal? y-status "replace single") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons r outy))]
					[(equal? y-status "do not replace") (argument-replacer x y r (cdr x3) (cdr y3) (cons (car x3) outx) (cons (car y3) outy))]
				)
			]
		)
	)
)
#|
(define (argument-replacer x y r x3 y3 outx outy)
	(if (list? x3)
		(if (empty? x3)
			(list (reverse outx) (reverse outy))
			(let ((xh (car x3)) (yh (car y3)))
				(cond
					[(equal? x xh)
						(cond
							[(equal? y yh) (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons r outy))]
							[(list? yh)
								(cond
									[(and (or (equal? (car yh) 'lambda) (equal? (car yh) 'λ)) (= (length yh) 3))
										(cond
											[(and (list? (cadr yh)) (member y (cadr yh)))
												(argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons yh outy))
											]
											[(equal? (cadr yh) y) (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons yh outy))]
											[else (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons (argument-replacer-single y r y3 '() ) outy))]
										)
									]
									[else (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons (argument-replacer-single y r yh '() ) outy))]
								)
							]
							[else (argument-replacer x y r (cdr x3) (cdr y3) (cons r outx) (cons yh outy))]
						)
					]
					[(list? xh)
						(cond
							[(and (or (equal? (car xh) 'lambda) (equal? (car xh) 'λ)) (= (length xh) 3))
								(if (list? yh)
									(cond
										[(and (or (equal? (car yh) 'lambda) (equal? (car yh) 'λ)) (= (length yh) 3))
											(argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons yh outy))
										]
										[else (argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons (argument-replacer-single y r yh '() ) outy))]
									)
									(if (equal? y yh)
										(argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons r outy))
										(argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons yh outy))
									)
								)
							]
							[(list? yh)
								(cond
									[(and (or (equal? (car yh) 'lambda) (equal? (car yh) 'λ)) (= (length yh) 3))
										(argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r xh '() ) outx) (cons yh outy))
									]
									[else (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r xh '() ) outx) (cons (argument-replacer-single y r yh '() ) outy))]
								)
							]
							[(equal? y yh) (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r xh '() ) outx) (cons r outy))]
							[else (argument-replacer x y r (cdr x3) (cdr y3) (cons (argument-replacer-single x r xh '() ) outx) (cons yh outy))]
						)
					]
					[else
						(cond
							[(equal? y yh) (argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons r outy))]
							[(list? yh)
								(cond
									[(and (or (equal? (car yh) 'lambda) (equal? (car yh) 'λ)) (= (length yh) 3))
										(argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons yh outy))
									]
									[else (argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons (argument-replacer-single y r yh '() ) outy))]
								)
							]
							[else (argument-replacer x y r (cdr x3) (cdr y3) (cons xh outx) (cons yh outy))]
						)
					]
				)
			)
		)
		(cond
			[(equal? x x3)
				(cond
					[(equal? y y3) (list r r)]
					[else (list r y3)]
				)
			]
			[else
				(cond
					[(equal? y y3) (list x3 r)]
					[else (list x3 y3)]
				)
			]
		)
	)
)
|#
(define (argument-comparer x2 y2 x3 y3 out)
	(if (empty? x2)
		(list (reverse out) (expr-compare x3 y3))
		(let ((xh (car x2)) (yh (car y2)))
			(cond
				[(equal? xh yh) (argument-comparer (cdr x2) (cdr y2) x3 y3 (cons xh out))]
				[else
					(let ((r (string->symbol (string-append (symbol->string xh) "!" (symbol->string yh)))))
						(let ((output (argument-replacer xh yh r x3 y3 '() '() )))
							(argument-comparer (cdr x2) (cdr y2) (car output) (cadr output) (cons r out))
						)
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
						(if (equal? (cadr x) (cadr y))
							(list x1 (cadr x) (expr-compare (cddr x) (cddr y)))
							(cons (list 'if '% (list x1 (cadr x)) (list y1 (cadr y))) (expr-compare (cddr x) (cddr y)))
						)
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

(define (test-expr-compare x y)
	(and
		(let ((comparison (expr-compare x y)))
			;((eval `(lambda (%) ,comparison) (variable-reference->namespace (#%variable-reference))) #t)
			(and
				(equal? (eval x (variable-reference->namespace (#%variable-reference))) ((eval `(lambda (%) ,comparison) (variable-reference->namespace (#%variable-reference))) #t))
				(equal? (eval y (variable-reference->namespace (#%variable-reference))) ((eval `(lambda (%) ,comparison) (variable-reference->namespace (#%variable-reference))) #f))
			)
		)
	)
)

#|
(eval '(cons 'a 'b) (variable-reference->namespace (#%variable-reference)))
((eval '(lambda (% x y) (if % x y)) (variable-reference->namespace (#%variable-reference))) #t 'a 'b)
(eval '(expr-compare '(cons 'a 'b) '(list 'a 'b)) (variable-reference->namespace (#%variable-reference)))
((eval '(lambda (% x y) (expr-compare x y)) (variable-reference->namespace (#%variable-reference))) #t '(cons 'a 'b) '(list 'a 'b))
(test-expr-compare '(cons 'a 'b) '(list 'a 'b))
|#

#|
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
|#
(expr-compare '(cons a lambda) '(cons a λ))
(expr-compare '(lambda (a) a) '(lambda (b) b))
(expr-compare '(lambda (a) b) '(cons (c) b))
(expr-compare '((lambda (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
(expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
(expr-compare ''lambda '(quote λ))
(expr-compare '(lambda (a b) a) '(λ (b) b))
(expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
(expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
(expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
(expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
