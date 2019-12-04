#lang racket
(require rackunit)

(define (num-has-pair? n)
  (define s (number->string n))
  (for/or ([a (in-list (string->list s))]
            [b (in-list (string->list (substring s 1)))])
    (eq? a b)))

(define (num-is-increasing? n)
  (apply char<=? (string->list (number->string n))))

(define (qualifies? n)
  (and (num-has-pair? n)
       (num-is-increasing? n)))

(module+ test
  (check-eq? (qualifies? 111111) #t)
  (check-eq? (qualifies? 223450) #f)
  (check-eq? (qualifies? 123789) #f))

(define (part1 from to)
  (for/fold ([count 0])
            ([n (in-range from to)]
             #:when (qualifies? n))
    (add1 count)))

(part1 134792 675810)

(define (num-has-exact-pair? n)
  (define groups (group-by values (string->list (number->string n))))
  (ormap (lambda (xs) (= (length xs) 2)) groups))

(define (qualifies2? n)
  (and (num-is-increasing? n)
       (num-has-exact-pair? n)))

(module+ test
  (check-eq? (qualifies2? 112233) #t)
  (check-eq? (qualifies2? 123444) #f)
  (check-eq? (qualifies2? 111122) #t))

(define (part2 from to)
  (for/fold ([count 0])
            ([n (in-range from to)]
             #:when (qualifies2? n))
    (add1 count)))

(part2 134792 675810)
