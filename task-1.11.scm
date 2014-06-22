(define (f-recursive n)
  (cond ((< n 3) n)
		(else (+ (f-recursive (- n 1))
				 (* 2 (f-recursive (- n 2)))
				 (* 3(f-recursive (- n 3)))))))

(define (f-iterative n)
  (define (iter fx-1 fx-2 fx-3 x)
	(cond ((< n x) n)
		  ((= n x) (+ fx-1
					  (* 2 fx-2)
					  (* 3 fx-3)))
		  (else (iter (+ fx-1
						 (* 2 fx-2)
						 (* 3 fx-3))
					  fx-1
					  fx-2
					  (+ x 1)))))
  (iter 2 1 0 3))
(begin
  (display (f-recursive 28))
  (newline)
  (display (f-iterative 28))
  (newline))
