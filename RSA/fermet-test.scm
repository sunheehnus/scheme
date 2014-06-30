(define (isprime? n times)
  (define (fermet-test n)
	(define (expmod base exp mod)
	  (define (mulmod x y)
		(remainder (* x y) mod))
	  (define (squmod x) (mulmod x x))
	  (define (iter res base exp)
		(cond ((= exp 0) res)
			  ((even? exp) (iter res (squmod base) (/ exp 2)))
			  (else (iter (mulmod res base) base (- exp 1)))))
	  (iter 1 base exp))
	(define (try-it a)
	  (= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) #t)
		((fermet-test n) (isprime? n (- times 1)))
		(else #f)))

(begin
  (display (isprime? 13 4))
  (newline)
  )