#lang racket

(require rackunit
         "../utils.rkt")

(module+ test
  (define test "123456789012")
  (check-equal? (layers test 3 2) '((1 2 3 4 5 6) (7 8 9 0 1 2))))

(define (layers input w h)
  (define nums (map (lambda (c)
                      (- (char->integer c)
                         (char->integer #\0)))
                    (string->list input)))
  (partition nums (* w h)))


(define (num-digits l n)
  (length (filter (curry = n) l)))

(define (fewest-0 layers)
  (car 
   (sort layers < #:key (lambda (l) (num-digits l 0)))))

(define (part1 input w h)
  (define layer (fewest-0 (layers input w h)))
  (* (num-digits layer 1) (num-digits layer 2)))

(part1 (file->string "input8.txt") 25 6)

(define (part2 input w h)
  (define ls (layers input w h))
  (define pixels (apply map list ls))
  (define rows
    (partition
     (for/list ([px (in-list pixels)])
       (ormap (lambda (p)
                (if (= p 2) #f p))
              px))
     w))
  (for ([row (in-list rows)])
    (for ([px (in-list row)])
      (if (= px 0)
          (printf "#")
          (printf " ")))
    (printf "~%")))

(part2 (file->string "input8.txt") 25 6)
