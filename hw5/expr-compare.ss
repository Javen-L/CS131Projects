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
									[(equal? x (cadr xh)) "list to not check" ]
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
					[(equal? y-status "single not equal") (list r y3)]
				)
			]
			[(equal? x-status "single not equal")
				(cond
					[(equal? y-status "single equal") (list x3 r)]
					[(equal? y-status "single not equal") (list x3 y3)]
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
(define (argument-comparer x2 y2 x3 y3 out)
	(if (empty? x2)
		(cons (reverse out) (expr-compare x3 y3))
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
	(let ((x1 (car x)) (y1 (car y)) (x2 (cadr x)) (y2 (cadr y)) (x3 (cddr x)) (y3 (cddr y)))
		(if (or (not (= (length x2) (length y2))) (and (list? x3) (not (list? y3))) (and (not (list? x3)) (list? y3)) (and (and (list? x3) (list? y3)) (not (= (length x3) (length y3)))))
			(list 'if '% x y)
			(if (or (equal? x1 'λ) (equal? y1 'λ))
				(cons 'λ (argument-comparer x2 y2 x3 y3 '()))
				(cons 'lambda (argument-comparer x2 y2 x3 y3 '()))
			)
		)
	)
)
(define (get-base out acc)
	(if (empty? out)
		(list (reverse acc) out)
		(if (equal? (car out) 'quote)
			(get-base (cdr out) (cons (car out) acc))
			(list (reverse (cons (car out) acc)) (cdr out))
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
						(let ((expr (expr-compare (cddr x) (cddr y))))
							(if (equal? expr '() )
								(list 'if '% (list x1 (cadr x)) (list y1 (cadr y)))
								(cons (list 'if '% (list x1 (cadr x)) (list y1 (cadr y))) expr)
							)
						)
					]
					;one starts with quote
					[(equal? x1 'quote)
						(let ((xout (get-base (cdr x) '() )))
							(cons (list 'if '% (list x1 (car xout)) y1) (expr-compare (cdr xout) (cdr y)))
						)
					]
					[(equal? y1 'quote)
						(let ((yout (get-base (cdr y) '() )))
							(cons (list 'if '% x1 (list y1 (car yout))) (expr-compare (cdr x) (cdr yout)))
						)
					]
					;check for lambda expression (replace lambda with λ)
					[(and (equal? x1 'λ) (>= (length x) 3))
						(cond
							[(and (or (equal? y1 'λ) (equal? y1 'lambda)) (>= (length y) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? y1 'λ) (>= (length y) 3))
						(cond
							[(and (or (equal? x1 'λ) (equal? x1 'lambda)) (>= (length x) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? x1 'lambda) (>= (length x) 3))
						(cond
							[(and (equal? y1 'lambda) (>= (length y) 3)) (lambda-parser x y)]
							[else (list 'if '% x y)]
						)
					]
					[(and (equal? y1 'lambda) (>= (length y) 3))
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
					[(and (list? x1) (list? y1))
						(cons (expr-compare x1 y1) (expr-compare (cdr x) (cdr y)))
					]
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
			;(eval `(let ((% #t)) ,comparison) (variable-reference->namespace (#%variable-reference)))
			(and
				(equal? (eval x (variable-reference->namespace (#%variable-reference))) (eval `(let ((% #t)) ,comparison) (variable-reference->namespace (#%variable-reference))))
				(equal? (eval y (variable-reference->namespace (#%variable-reference))) (eval `(let ((% #f)) ,comparison) (variable-reference->namespace (#%variable-reference))))
			)
		)
	)
)

(define test-expr-x
	'(let ((x3 (list 'x 'x3t)))
		(if (list? x3)
			(if (empty? x3)
				"empty list"
				((lambda (xh)
					(cond
						[(list? xh)
							(cond
								[(and (or (equal? (car xh) 'lambda) (equal? (car xh) 'λ)) (= (length xh) 3))
									(cond
										[(and (list? (cadr xh)) (member 'x (cadr xh)))
											"list to not check"
										]
										[(equal? 'x (cadr xh)) "list to not check" ]
										[else "list to check"]
									)
								]
								[else "list to check"]
							)
						]
						[(equal? 'x xh) "replace single"]
						[else "do not replace"]
					)
				) (car x3))
			)
			(if (equal? 'x x3)
				"single equal"
				"single not equal"
			)
		)
	)
)

(define test-expr-y
	'(let ((x3 'y))
		(if (list? x3)
			(if (equal? x3 '())
				"empty list"
				((lambda (yh)
					(cond
						[(list? yh)
							(cond
								[(and (or (equal? (car yh) 'λ) (equal? (car yh) 'lambda)) (= (length yh) 3))
									(cond
										[(and (list? (cadr yh)) (member 'y (cadr yh)))
											"list not to check"
										]
										[(equal? 'y (cadr yh)) "list not to check" ]
										[else "list to check"]
									)
								]
								[else "list to check"]
							)
						]
						[(equal? 'y yh) "replace single"]
						[else "do not replace single"]
					)
				) (car x3))
			)
			(if (equal? 'y x3)
				"single equal"
				"single is not equal"
			)
		)
	)
)

#|
;personal test cases
(expr-compare '() '())
; '()
(expr-compare 'x '())
; if % x '()
(expr-compare '(x y) '())
; if % '(x y) '()
(expr-compare '(lambda (if) (if x y)) '(lambda (else) (else x y)))
; '(lambda (if!else) (if!else x y))
;lambda redefinition overrides if meaning
(expr-compare '(''''''x y) '(''''''y x))
; '((if % ''''''x ''''''y) (if % y x))
(expr-compare '(lambda (x y) x y) '(lambda (y z) y z))
; '(lambda (x!y y!z) x!y y!z)
(expr-compare test-expr-x test-expr-y)
; '(let ((x3 (if % (list 'x 'x3t) 'y))) (if (list? x3) (if (if % (empty? x3) (equal? x3 '())) "empty list" ((lambda (xh!yh) (cond ((list? xh!yh) (cond ((and (or (equal? (car xh!yh) (if % 'lambda 'λ)) (equal? (car xh!yh) (if % 'λ 'lambda))) (= (length xh!yh) 3)) (cond ((and (list? (cadr xh!yh)) (member (if % 'x 'y) (cadr xh!yh))) (if % "list to not check" "list not to check")) ((equal? (if % 'x 'y) (cadr xh!yh)) (if % "list to not check" "list not to check")) (else "list to check"))) (else "list to check"))) ((equal? (if % 'x 'y) xh!yh) "replace single") (else (if % "do not replace" "do not replace single")))) (car x3))) (if (equal? (if % 'x 'y) x3) "single equal" (if % "single not equal" "single is not equal"))))
|#

#|
(test-expr-compare '(cons 'a 'b) '(list 'a 'b))
(test-expr-compare '(cons 'a 'b) '(cons 'a 'b))
(test-expr-compare test-expr-x test-expr-y)
(test-expr-compare '((lambda (lambda) (+ lambda (+ lambda 1))) 3)
              '((lambda (if) (+ if (+ if 1))) 3))
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
|#
