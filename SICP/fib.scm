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

(define (FIB-lgN n)
  (define (nextp p q)
	(+ (* p p) (* q q)))
  (define (nextq p q)
	(+ (* p q 2) (* q q)))
  (define (nexta a b p q)
	(+ (* b q) (* a q) (* a p)))
  (define (nextb a b p q)
	(+ (* b p) (* a q)))
  (define (fib a b p q count)
	(cond ((= count 0) b)
		  ((even? count) (fib a b (nextp p q) (nextq p q) (/ count 2)))
		  (else (fib (nexta a b p q) (nextb a b p q) p q (- count 1)))
		  )
	)
  (fib 1 0 0 1 n)
  )


(begin
  (display (FIB-recursive 42))
  (newline)
  (display (FIB-iterative 42))
  (newline)
  (display (FIB-lgN 42))
  (newline)
  )