(define (sum term next floor ceil)
  (cond ((> floor ceil) 0)
		(#t (+ (term floor) (sum term next (next floor) ceil)))))


(define (sum-cube floor ceil)
  (define (inc n) (+ n 1))
  (sum (lambda(x) (* x x x)) inc floor ceil))

(define (sum-integers floor ceil)
  (define (inc n) (+ n 1))
  (sum (lambda(x) x) inc floor ceil))

(define (pi-sum floor ceil)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	(+ x 4))
  (sum pi-term pi-next floor ceil))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b) dx))

(display (sum-cube 1 10))
(newline)
(display (sum-integers 1 10))
(newline)
(display (* 8 (pi-sum 1 1000)))
(newline)
(define (cube x) (* x x x))
(display (integral cube 0 1 0.01))
(newline)
