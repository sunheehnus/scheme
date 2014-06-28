(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (combiner (cond ((filter a) (term a))
									 (#t null-value))
							   result))))
  (iter a null-value))

(define (prime_n-sum n)
  (define (GCD a b)
	(if (= b 0)
	  a
	  (GCD b (remainder a b))))
  (define (prime_n? a)
	(= (GCD a n) 1))
  (define (self x) x)
  (define (inc x) (+ x 1))
  (filtered-accumulate prime_n? + 0 self 1 inc n))
