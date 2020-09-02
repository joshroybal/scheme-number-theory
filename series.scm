(load "lib")
(define reciprocal (lambda (x) (/ 1. x)))
(define square (lambda (x) (* x x)))
(define cube (lambda (x) (* x x x)))

(define make-series-list
  (lambda (term a b)
    (make-series-list-iter term a b 0 '())))

(define make-series-list-iter
  (lambda (term a b acc ls)
    (if (> a b)
	ls
	(let ((m (+ acc (term a))))
	  (make-series-list-iter term (+ a 1) b m (append ls (list m)))))))

(define cumulative-list
  (lambda (ls)
    (cumulative-list-iter ls '() 0)))

(define cumulative-list-iter
  (lambda (oldls newls acc)
    (if (null? oldls)
	newls
	(let ((m (+ acc (car oldls))))
	  (cumulative-list-iter (cdr oldls) (append newls (list m))
				(+ acc (car oldls)))))))

(define harm-series
  (make-series-list (lambda(x)(reciprocal x)) 1 1000))

(define sqr-series
  (make-series-list (lambda(x)(reciprocal (square x))) 1 1000))

(define cube-series
  (make-series-list (lambda(x)(reciprocal (cube x))) 1 1000))

(define tri-series
  (make-series-list (lambda(x)(reciprocal (tri x))) 1 1000))

(define fib-series
  (make-series-list (lambda(x)(reciprocal (fib x))) 1 1000))

(define bin-series
  (make-series-list (lambda(x)(reciprocal (expt 2 x))) 0 999))

(define prime-series
  (cumulative-list (reciprocals (prime-list 1000))))

(define twin-series
  (cumulative-list (reciprocals (twin-prime-list 500))))

; emit multiple lists
(define-syntax foreach
 (syntax-rules ()
  ((_ ((variables lists) ...)
    body ...)
   (for-each (lambda (variables ...) body ...) lists ...))))

(define dump-lists
  (lambda (harm-series
	   prime-series
	   fib-series
	   bin-series
	   tri-series
	   twin-series
	   sqr-series
	   cube-series
	   out-file)
    (foreach ((i harm-series)
	      (j prime-series)
              (k fib-series)
	      (m bin-series)
	      (n tri-series)
	      (p twin-series)
	      (q sqr-series)
	      (r cube-series))
             (write i out-file) (write-char #\Space out-file) 
             (write j out-file) (write-char #\Space out-file)
	     (write k out-file) (write-char #\Space out-file)
	     (write m out-file) (write-char #\Space out-file)
	     (write n out-file) (write-char #\Space out-file)
	     (write p out-file) (write-char #\Space out-file)
	     (write q out-file) (write-char #\Space out-file)
             (write r out-file) (newline out-file))))


(define out-file (open-output-file "series.dat"))
(dump-lists harm-series prime-series fib-series bin-series tri-series
	    twin-series sqr-series cube-series out-file)
(close-output-port out-file)
