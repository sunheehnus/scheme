#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

(define (split big-small2-func small-small-func)
  (lambda (painter n)
	(if (= n 0)
	  painter
	  (let ((smaller ((split big-small2-func small-small-func) painter (- n 1))))
		(big-small2-func painter
						 (small-small-func smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 4))
(paint (up-split einstein 4))
