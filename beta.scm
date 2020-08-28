;(define ln (init-list 1000))
;(define ls (map(lambda(n)(* n 1000))(init-list 1000)))
(define ln (map (lambda (n) (* n 1000)) (init-list 1000)))
(define lcp (map-primes-counts ln))
(define nl (map (lambda(n)(/ n (log n))) ln))

; emit multiple lists
(define-syntax foreach
 (syntax-rules ()
  ((_ ((variables lists) ...)
    body ...)
   (for-each (lambda (variables ...) body ...) lists ...))))

(define dump-lists
  (lambda (ln lcp nl out-file)
    (foreach ((i ln)
              (p lcp)
              (n nl))
             (write i out-file) (write-char #\Space out-file) 
             (write p out-file) (write-char #\Space out-file) 
             (write n out-file) (newline out-file))))


(define out-file (open-output-file "emp.dat"))
;(foreach ((i ls)
;          (p np)
;          (n nl))
 ;(write (list p n) out)
; (write i out) (write-char #\Space out) 
; (write p out) (write-char #\Space out) 
; (write n out) (newline out))
(dump-lists ln lcp nl out-file)
(close-output-port out-file)
