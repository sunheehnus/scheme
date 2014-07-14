(define (cont-frac n d k)
  (define (iter cnt result)
	(if (= cnt 0)
	  result
	  (iter (- cnt 1) (/ (n cnt) (+ (d cnt) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(display (/ 1 (cont-frac (lambda (i) 1.0)
				(lambda (i) 1.0)
				1000)))
