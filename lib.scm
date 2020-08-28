; list initialization procedures
(define init-list
  (lambda (n)
    (init-list-iter n ())))

(define init-list-iter
  (lambda (n ls)
    (if (= n 0)
      ls
      (init-list-iter (- n 1) (cons n ls)))))

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
  (= n (smallest-divisor n)))

; procedures construct list of the first n primes
(define prime-list
  (lambda (n)
    (prime-list-iter n 2 '())))

(define prime-list-iter
  (lambda (n m ls)
    (if (= 0 n)
      ls
      (if (prime? m)
        (prime-list-iter (- n 1) (+ m 1) (cons m ls))
        (prime-list-iter n (+ m 1) ls)))))

; procedures construct list of primes p <= n
(define primes
  (lambda (n)
    (primes-iter n 2 '())))

(define primes-iter
  (lambda (n m ls)
    (if (> m n)
      ls
      (if (prime? m)
        (primes-iter n (+ m 1) (cons m ls))
        (primes-iter n (+ m 1) ls)))))


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
