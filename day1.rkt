#lang racket
(require rackunit
         threading)

(define (calc-fuel mass)
  (max 0 
       (- (quotient mass 3) 2)))

(module+ test
  (check-eq? (calc-fuel 12) 2)
  (check-eq? (calc-fuel 14) 2)
  (check-eq? (calc-fuel 100756) 33583))

(define (read-input name calculator)
  (for/fold ([sum 0])
            ([line (in-list (file->lines name))])
    (+ sum (calculator (string->number line)))))

(define (calc-fuel2 mass)
  (let loop ([total 0]
             [add mass])
    (define fuel (calc-fuel add))
    (if (= fuel 0)
        total
        (loop (+ total fuel) fuel))))


(module+ test
  (check-eq? (calc-fuel2 1969) 966)
  (check-eq? (calc-fuel2 100756) 50346))

(read-input "input1.txt" calc-fuel)
(read-input "input1.txt" calc-fuel2)



