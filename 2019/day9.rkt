#lang racket

(require rackunit
         "../intcode.rkt")


(module+ test
  (define test (new-prog "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
  (check-equal? (run! test)
                '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))

  (define test2 (new-prog "1102,34915192,34915192,7,4,7,99,0"))
  (check-eq? (car (run! test2)) 1219070632396864)

  (define test3 (new-prog "104,1125899906842624,99"))
  (check-eq? (car (run! test3)) 1125899906842624))

(define input (new-prog (file->string "input9.txt")))

(restart! input)
(run! input (list 1))

(restart! input)
(run! input (list 2))


