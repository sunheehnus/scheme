(define (fermet-test-once n a)
  (define (expmod base exp mod)
	(define (mulmod x y)
	  (remainder (* x y) mod))
	(define (squmod x) (mulmod x x))
	(define (compute-iter res base exp)
	  (cond ((= exp 0) res)
			((even? exp) (compute-iter res (squmod base) (/ exp 2)))
			(else (compute-iter (mulmod res base) base (- exp 1)))))
	(compute-iter 1 base exp))
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it a))

(define (pass-all-fermet-test n)
  (define (iter a)
	(cond ((>= a n) #t)
		  ((fermet-test-once n a) (iter (+ a 1)))
		  (else #f)))
  (iter 2))
(begin
  (display (pass-all-fermet-test 561))
  (newline)
  (display (pass-all-fermet-test 1105))
  (newline)
  (display (pass-all-fermet-test 1729))
  (newline)
  (display (pass-all-fermet-test 2465))
  (newline)
  (display (pass-all-fermet-test 2821))
  (newline)
  (display (pass-all-fermet-test 6601))
  (newline)
  )
