#lang racket
(require "../utils.rkt")

(define test (list 
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576))

(define (find-invalid nums preamble)
  (let loop ([nums nums])
    (define-values (prev rest) (split-at nums preamble))
    (if (empty? rest)
        #f
        (let ([sums (list->set (map (lambda (c) (apply + c)) (combinations prev 2)))])
          (if (set-member? sums (car rest))
              (loop (cdr nums))
              (car rest))))))

(module+ test
  (require rackunit)
  (check-equal? (find-invalid test 5) 127))

(define input (map string->number (file->lines "input9.txt")))
(find-invalid input 25)

;; part 2

(define (find-rolling-sum nums target)
  (define window (deque (car nums)))
  (let loop ([sum (car nums)]
             [todo (cdr nums)])
    (cond
      [(empty? todo) #f]
      [(= sum target) (deque->list window)]
      [(< sum target) (push-end! window (car todo)) (loop (+ sum (car todo)) (cdr todo))]
      [(> sum target) (loop (- sum (pop-front! window)) todo)])))

(define (part2 nums preamble)
  (define target (find-invalid nums preamble))
  (define window (find-rolling-sum nums target))
  (+ (apply min window) (apply max window)))

(module+ test
  (check-equal? (part2 test 5) 62))

(part2 input 25)

