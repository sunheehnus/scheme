(define (cont-frac n d k)
  (define (iter cnt result)
	(if (= cnt 0)
	  result
	  (iter (- cnt 1) (/ (n cnt) (+ (d cnt) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (- (* x x)))
				(lambda (i) (- (* i 2) 1))
				k)
	 (- x)))

(define (pi)
  (define ceil 10000)
  (define (product term a next b)
	(define (iter a result)
	  (if (> a b)
		result
		(iter (next a) (* result (term a)))))
	(iter a 1))
  (define (square x) (* x x))
  (define (next x) (+ x 2))
  (* (/ (product square 4 next ceil) ceil (product square 3 next ceil)) 2.0 4))

(display (tan-cf (/ (pi) 4) 10000))
(newline)
