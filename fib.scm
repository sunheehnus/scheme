(define (FIB N)
  (if (< N 2)
      N
      (+ (FIB (- N 1))
	 (FIB (- N 2)))))
(begin
  (display (FIB 10))
  (newline)
  )
