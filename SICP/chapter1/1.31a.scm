(define (product term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (self x) x)
  (product self 1 inc n))

(define (pi)
  (define ceil 10000)
  (define (square x) (* x x))
  (define (next x) (+ x 2))
  (* (/ (product square 4 next ceil) ceil (product square 3 next ceil)) 2.0 4))

(display (pi))
(newline)
