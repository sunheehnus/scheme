(define (cont-frac n d k)
  (define (iter cnt result)
	(if (= cnt 0)
	  result
	  (iter (- cnt 1) (/ (n cnt) (+ (d cnt) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (build-e k)
  (+ (cont-frac (lambda (i) 1.0)
				(lambda (i) (let ((rem (remainder i 3))
								  (div (floor (/ i 3))))
							  (cond ((= rem 2) (* (+ div 1) 2))
									(#t 1))))
				k)
	 2))

(display (build-e 10000))
