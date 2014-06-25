(define (isprime? n)
  (define (next x)
	(if (= x 2)
	  3
	  (+ x 2)))
  (define (find-divisor x div) ;find div that can be divided by x
	(define (square x)
	  (* x x))
	(cond ((= (remainder x div) 0) div)
		  ((> (square div) x) x)
		  (else (find-divisor x (next div)))))
  (define (smallest-divisor x)
	(find-divisor x 2))
  (= (smallest-divisor n) n))

(display (isprime? 113))
