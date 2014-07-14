(define (MOVE N FROM TO SPARE)
  (cond ((= N 0))
		(else
		  (MOVE (- N 1) FROM SPARE TO)
		  (display (string-append "MOV "
								  (number->string N)
								  " FROM "
								  FROM
								  " TO "
								  TO))
		  (newline)
		  (MOVE (- N 1) SPARE TO FROM))))
(MOVE 4 "start" "target" "tmp")
