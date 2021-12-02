#lang racket



(define (parse-line s)
  (define m (regexp-match (pregexp "(\\d+)-(\\d+) (\\w): (\\w+)") s))
  (match m
    [(list _ min max c pw) (values (string->number min) (string->number max) (string-ref c 0) pw)]))

(define (occurrences s c)
  (for/sum ([ch (in-string s)]
            #:when (eq? ch c))
    1))

(define (valid-pw? line)
  (define-values (min max c pw) (parse-line line))
  (define n (occurrences pw c))
  (and (>= n min) (<= n max)))

(module+ test
  (require rackunit)
  (define passwords (list "1-3 a: abcde"
"1-3 b: cdefg"
"2-9 c: ccccccccc"))
  (check-equal? (map valid-pw? passwords) (list #t #f #t))
  (check-equal? (length (filter valid-pw? passwords)) 2))

(define passwords (file->lines "input2.txt"))
(length (filter valid-pw? passwords))

(define (valid2? line)
  (define-values (p1 p2 c pw) (parse-line line))
  (xor (eq? c (string-ref pw (sub1 p1)))
       (eq? c (string-ref pw (sub1 p2)))))

(module+ test
  (check-equal? (map valid2? passwords) (list #t #f #f)))

(length (filter valid2? passwords))
