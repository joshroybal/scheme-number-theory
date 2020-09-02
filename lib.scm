; list initialization procedures
(define init-list
  (lambda (n)
    (define iter
      (lambda (n ls)
        (if (= n 0)
          ls
          (iter (- n 1) (cons n ls)))))
    (iter n ())))

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
    (define iter
      (lambda (n m ls)
        (if (= 0 n)
          ls
          (if (prime? m)
            (iter (- n 1) (+ m 1) (append ls (list m)))
            (iter n (+ m 1) ls)))))
    (iter n 0 '())))

; procedures construct list of the first n twin primes
(define twin-prime-list
  (lambda (n)
    (define iter
      (lambda (n m ls)
        (if (= 0 n)
          ls
          (if (and (prime? m) (prime? (+ m 2)))
            (iter (- n 1) (+ m 2) (append ls (list m (+ m 2))))
            (iter n (+ m 2) ls)))))
    (iter n 3 '())))

; procedures construct list of primes p <= n
;(define primes
;  (lambda (n)
;    (define iter
;      (lambda (n ls)
;        (if (= n 0)
;          ls
;          (if (prime? n)
;            (iter (- n 1) (cons n ls))
;            (iter (- n 1) ls)))))
;    (if (odd? n)
;      (iter n '())
;      (iter (- n 1) '()))))

; prime counting procedures (no. of primes p <= n)
(define count-primes
  (lambda (n)
    (define iter
      (lambda (n acc)
        (if (= 1 n)
          acc
          (if (prime? n)
            (iter (- n 1) (+ acc 1))
            (iter (- n 1) acc)))))
    (if (< n 2)
      0
      (iter n 0))))

; general summation procedure
(define sum-list
  (lambda (ls)
    (define iter
      (lambda (ls acc)
        (if (null? ls)
          acc
          (iter (cdr ls) (+ acc (car ls))))))
    (iter ls 0)))

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
(define tri
  (lambda (n)
    (define iter
      (lambda (n acc)
        (if (= 0 n)
          acc
          (iter (- n 1) (+ acc n)))))
    (iter n 0)))

; useful for more than Fibonacci nos.
(define fib
  (lambda (n)
    (define iter
      (lambda (a b count)
        (if (= count 0)
          b
          (iter (+ a b) a (- count 1)))))
    (iter 1 0 n)))

; another nth harmonic no. procedure
(define harm
  (lambda (n)
    (define iter
      (lambda (n acc)
        (if (= 0 n)
          acc
          (iter (- n 1) (+ acc (reciprocal n))))))
    (iter n 0)))

; sum of the reciprocals of the first n primes 
(define harm-prime
  (lambda (n)
    (define iter
      (lambda (n m acc)
        (if (= 0 n)
          acc
          (if (prime? m)
            (iter (- n 1) (+ m 2) (+ acc (reciprocal m)))
            (iter n (+ m 2) acc)))))
    (cond ((< n 1)
           0)
          ((= n 1)
           .5)
          (else
            (iter (- n 1) 3 .5)))))

; sum of the reciprocals of the first n twin-primes
(define reciprocal (lambda(x)(/ 1. x)))

(define brun
  (lambda (n)
    (define iter
      (lambda (n m acc)
        (if (= 0 n)
          acc
          (if (and (prime? m) (prime? (+ m 2)))
            (let ((t0 (reciprocal m)) (t2 (reciprocal (+ m 2))))
              (iter (- n 1) (+ m 6) (+ acc t0 t2)))
            (iter n (+ m 6) acc)))))
    (let ((b1 (apply + (map reciprocal '(3 5))))
          (b2 (apply + (map reciprocal '(3 5 5 7)))))
      (cond ((< n 1)
             0)
            ((= n 1)
             b1)
            ((= n 2)
             b2)
            (else
              (iter (- n 2) 11 b2))))))
