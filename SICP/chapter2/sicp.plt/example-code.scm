#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

(define (right-split painter n)
  (if (= n 0)
	painter
	(let ((smaller (right-split painter (- n 1))))
	  (beside painter
			  (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
	painter
	(let ((smaller (up-split painter (- n 1))))
	  (below painter
			 (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
	painter
	(let ((up (up-split painter (- n 1)))
		  (right (right-split painter (- n 1))))
	  (let ((top-left (beside up up))
			(bottom-right (below right right))
			(corner (corner-split painter (- n 1))))
		(beside (below painter top-left)
				(below bottom-right corner))))))

;(define (flipped-pairs painter)
  ;(let ((painter2 (beside painter (flip-vert painter))))
	;(below painter2 painter2)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
	(let ((top (beside (tl painter) (tr painter)))
		  (bottom (beside (bl painter) (br painter))))
	  (below bottom top))))

;(define (flipped-pairs painter)
  ;((square-of-four identity flip-vert identity flip-vert) painter))

(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  ((square-of-four flip-horiz identity
				   rotate180 flip-vert) (corner-split painter n)))

(paint (square-limit einstein 3))
