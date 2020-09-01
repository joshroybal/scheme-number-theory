; utility list
(define square (lambda(x)(* x x)))

; list initialization procedures
(define init-list
  (lambda (n)
    (init-list-iter n '())))

(define init-list-iter
  (lambda (n ls)
    (if (= n 0)
      ls
      (init-list-iter (- n 1) (cons n ls)))))

(define reciprocals
  (lambda (ls)
    (map (lambda(n)(/ 1. n)) ls)))

; list utility procedures

(define scale-list
  (lambda (n ls)
    (map (lambda(x)(* n x)) ls)))
  
; prime number procedures  
; auxiliary procedure to main primality test procedure
(define (smallest-divisor n)
  (if (= 0 (modulo n 2))
    2
    (if (= 0 (modulo n 3))
	3
	(find-divisor n 5))))

; main divisor search test procedure
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ((divides? (+ test-divisor 2) n) test-divisor)
        (else (find-divisor n (+ test-divisor 6)))))

(define (divides? a b)
  (= (remainder b a) 0))

; main primality test procedure
(define (prime? n)
  (if (> n 1)
      (= n (smallest-divisor n))
      #f))

; procedures construct list of the first n primes
(define prime-list
  (lambda (n)
    (prime-list-iter (- n 1) 3 (list 2))))

(define prime-list-iter
  (lambda (n m ls)
    (if (= 0 n)
	ls
	(if (prime? m)
	    (prime-list-iter (- n 1) (+ m 2) (append ls (list m)))
	    (prime-list-iter n (+ m 2) ls)))))

; procedures construct list of the first n twin primes
(define twin-prime-list
  (lambda (n)
    (twin-prime-list-iter (- n 2) 11 (list 3 5 5 7))))

(define twin-prime-list-iter
  (lambda (n m ls)
    (if (= 0 n)
      ls
      (if (and (prime? m) (prime? (+ m 2)))
        (twin-prime-list-iter (- n 1) (+ m 6) (append ls (list m (+ m 2))))
        (twin-prime-list-iter n (+ m 6) ls)))))

; procedures construct list of primes p <= n
(define primes
  (lambda (n)
    (if (odd? n)
	(primes-iter n '())
	(primes-iter (- n 1) '()))))

(define primes-iter
  (lambda (n ls)
    (if (= n 0)
      ls
      (if (prime? n)
        (primes-iter (- n 1) (cons n ls))
        (primes-iter (- n 1) ls)))))


; prime counting procedures (no. of primes p <= n)
(define count-primes
  (lambda (n)
    (count-primes-iter n 0)))

(define count-primes-iter
  (lambda (n acc)
    (if (= 1 n)
      acc
      (if (prime? n)
        (count-primes-iter (- n 1) (+ acc 1))
        (count-primes-iter (- n 1) acc)))))

; auxiliary prime-number counting procedures
; > (count-primes-list 5)) -> (0 0 1 2 2)
(define count-primes-list
  (lambda (n)
    (define lsp (primes n))
      (count-primes-list-iter lsp n '())))

(define count-primes-list-iter
  (lambda (lp n lpi)
    (if (= n 0)
      lpi
      (let ((m (- n 1)))
      (count-primes-list-iter lp m (cons (count lp m 0) lpi))))))

; yields no. of primes <= n
; e.g.
; > (count '(2 3 5) 4 0) -> 2
(define count
  (lambda (lp n acc)
    (if (null? lp)
      acc
      (if (<= (car lp) n)
        (count (cdr lp) n (+ acc 1))
        (count (cdr lp) n acc)))))

; assumes a list of natural numbers in ascending order wo duplicates
; e.g.
; > map-primes-counts '(10000 20000 30000 40000)) -> (1229 2262 3245 4203)
(define map-primes-counts
  (lambda (ln)
    (define lsp (primes (last ln)))
      (map-primes-counts-iter lsp ln '())))

(define map-primes-counts-iter
  (lambda (lp ln lpi)
    (if (null? ln)
      lpi
      (let ((tail (list (count lp (car ln) 0))))
      (map-primes-counts-iter lp (cdr ln) (append lpi tail))))))

; general summation procedure
(define sum-list
  (lambda (ls)
    (sum-list-iter ls 0)))

(define sum-list-iter
  (lambda (ls acc)
    (if (null? ls)
	acc
	(sum-list-iter (cdr ls) (+ acc (car ls))))))

; abstract summation procedure from SICP
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))

; harmonic numbers procedures
(define sum-harmonic
  (lambda (a b)
  (sum reciprocal a inc b)))

(define reciprocal
  (lambda (n)
    (/ 1. n)))

(define nth-harmonic
  (lambda (n)
    (sum-harmonic 1 n)))
				       
; useful for more than triangular nos.
(define triangular
  (lambda (n)
    (triangular-iter n 0)))

(define triangular-iter
  (lambda (n acc)
    (if (= 0 n)
	acc
	(triangular-iter (- n 1) (+ acc n)))))

; useful for more than Fibonacci nos.
(define fib
  (lambda (n)
  (fib-iter 1 0 n)))

(define fib-iter
  (lambda (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))

; another nth harmonic no. procedure
(define harm
  (lambda (n)
    (harm-iter n 0)))

(define harm-iter
  (lambda (n acc)
    (if (= 0 n)
        acc
        (harm-iter (- n 1) (+ acc (reciprocal n))))))

; sum of the reciprocals of the first n primes 
(define harm-prime
  (lambda (n)
    (cond ((< n 1)
           0)
           ((= n 1)
           .5)
           (else
            (harm-prime-iter (- n 1) 3 .5)))))

(define harm-prime-iter
  (lambda (n m acc)
    (if (= 0 n)
	acc
	(if (prime? m)
	    (harm-prime-iter (- n 1) (+ m 2) (+ acc (reciprocal m)))
	    (harm-prime-iter n (+ m 2) acc)))))

; sum of the reciprocals of the first n twin-primes
(define reciprocal (lambda(x)(/ 1. x)))

(define brun
  (lambda (n)
    (let ((b1 (apply + (map reciprocal '(3 5))))
          (b2 (apply + (map reciprocal '(3 5 5 7)))))
    (cond ((< n 1)
           0)
          ((= n 1)
           b1)
          ((= n 2)
           b2)
          (else
            (brun-iter (- n 2) 11 b2))))))

(define brun-iter
  (lambda (n m acc)
    (if (= 0 n)
	acc
	(if (and (prime? m) (prime? (+ m 2)))
            (let ((t0 (reciprocal m)) (t2 (reciprocal (+ m 2))))
	    (brun-iter (- n 1) (+ m 6) (+ acc t0 t2)))
	    (brun-iter n (+ m 6) acc)))))