(define (FIB-recursive N)
  (if (< N 2)
	N
	(+ (FIB-recursive (- N 1))
	   (FIB-recursive (- N 2)))))

(define (FIB-iterative N)
  (define (fib Xn-2 Xn-1 X)
	(cond ((< N X) N)
		  ((= X N) (+ Xn-2 Xn-1))
		  (else (fib Xn-1 (+ Xn-2 Xn-1) (+ X 1)))))
  (fib 0 1 2))

(begin
  (display (FIB-recursive 42))
  (newline)
  (display (FIB-iterative 42))
  (newline)
  )
