#lang racket

(require rackunit
         "../intcode.rkt")


(define (run-chain prog phases)
  (for/fold ([in 0])
            ([phase (in-list phases)])
    (restart! prog)
    (car (run! prog (list phase in)))))


(define (find-max prog)
  (for/fold ([m -inf.0])
            ([phases (in-list (permutations (list 0 1 2 3 4)))])
    (max m (run-chain prog phases))))


(module+ test
  (define test (new-prog "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
  (check-equal? (find-max test) 43210.0)

  (define test2 (new-prog "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0"))
  (check-equal? (find-max test2) 54321.0))

(define input (new-prog (file->string "input7.txt")))
(find-max input)

(define (run-chain2 progs phases #:debug [debug #f])
  (define a (car progs))
  (let loop ([chain progs]
             [in (list 0)])
    (if (empty? chain)
        (if (prog-done a)
            in
            (loop progs in))
        (let ([prog (car chain)])
          ;; The first run we use phase an input
          (when (not (empty? phases))
            (set! in (cons (car phases) in))
            (set! phases (cdr phases)))
          (define out (run! prog in #:debug debug))
          (loop (cdr chain) out)))))

(define (find-max2 input #:debug [debug #f])
  (define chain (for/list ([_ (in-range 5)]) (new-prog input)))
  (for/fold ([m -inf.0])
            ([phases (in-permutations (list 5 6 7 8 9))])
    (map restart! chain)
    (max m (car (run-chain2 chain phases #:debug debug)))))

(module+ test
  (check-equal? (find-max2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
                139629729.0)

  (check-equal? (find-max2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
                18216.0))

(find-max2 (file->string "input7.txt"))





