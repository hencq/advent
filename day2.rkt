#lang racket

(require rackunit)

(define test "1,9,10,3,
              2,3,11,0,
              99,
              30,40,50")

(define (input->vector input)
  (list->vector
   (map (compose string->number string-trim) (string-split input ","))))

(define (op vec pc fun)
  (define a (vector-ref vec (+ pc 1)))
  (define b (vector-ref vec (+ pc 2)))
  (define c (vector-ref vec (+ pc 3)))
  (vector-set! vec c (fun (vector-ref vec a) (vector-ref vec b)))
  vec)

(module+ test
  (define test-input "1,9,10,3,
              2,3,11,0,
              99,
              30,40,50")
  (define test-vec (input->vector test-input))
  (op test-vec 0 +)
  (check-eq? (vector-ref test-vec 3) 70)
  (op test-vec 4 *)
  (check-eq? (vector-ref test-vec 0) 3500))

(define (run-prog prog)
  (let loop ([pc 0])
    (cond
      [(= (vector-ref prog pc) 99) prog]
      [(= (vector-ref prog pc) 1) (op prog pc +) (loop (+ pc 4))]
      [(= (vector-ref prog pc) 2) (op prog pc *) (loop (+ pc 4))])))

(define (run input)
  (define prog (input->vector input))
  (run-prog prog))

(module+ test
  (check-equal? (run test-input) (vector 3500 9 10 70 2 3 11 0 99 30 40 50))
  (check-equal? (run "1,0,0,0,99") (vector 2 0 0 0 99))
  (check-equal? (run "2, 4, 4, 5, 99, 0") (vector 2 4 4 5 99 9801)))

(define (part1 fname)
  (define input (file->string fname))
  (define prog (input->vector input))
  (vector-set! prog 1 12)
  (vector-set! prog 2 2)
  (run-prog prog)
  (vector-ref prog 0))

(part1 "input2.txt")

(define (run-params prog0 noun verb)
  (define prog (vector-copy prog0))
  (vector-set! prog 1 noun)
  (vector-set! prog 2 verb)
  (run-prog prog)
  (vector-ref prog 0))


(define (part2 fname goal)
  (define prog0 (input->vector (file->string fname)))
  (let loop ([noun 0]
             [verb 0])
    (define result (run-params prog0 noun verb))
    (cond
      [(= result goal) (+ (* noun 100) verb)]
      [(= noun 100) #f]
      [(= verb 100) (loop (add1 noun) 0)]
      [else (loop noun (add1 verb))])))

(module+ test
  (check-eq? 
   (part2 "input2.txt" 3850704) 1202))

(part2 "input2.txt" 19690720)


