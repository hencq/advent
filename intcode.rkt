#lang racket

(require rackunit)

(provide run)

;; Some intcode utilities to (hopefully) help with later puzzles


;; A little mini assembler to help test things

(struct reg (i) #:transparent)
(struct ptr (addr) #:transparent)

(define (normalize-addrs code consts regs addrs)
  (for/fold ([code code]
             [consts consts]
             [regs regs])
            ([addr (in-list addrs)])
    (values
     (cons addr code)
     (if (number? addr)
         (set-add consts addr)
         consts)
     (if (reg? addr)
         (set-add regs addr)
         regs))))

(define (link code consts regs)
  (define code-len (length code))
  (define const-map (for/hash ([c (in-list consts)]
                               [i (in-naturals code-len)])
                      (values c i)))
  (define reg-map (for/hash ([c (in-list regs)]
                             [i (in-naturals (+ code-len (length consts)))])
                    (values c i)))
  (append
   (for/list ([c (in-list code)])
     (cond
       [(eq? c 'add) 1]
       [(eq? c 'mul) 2]
       [(eq? c 'halt) 99]
       [(reg? c) (hash-ref reg-map c)]
       [(number? c) (hash-ref const-map c)]
       [(ptr? c) (ptr-addr c)]))
   consts
   (make-list (length regs) 0)))

(define (asm instrs)
  (list->vector 
   (for/fold ([code '()]
              [consts '()]
              [regs '()]
              #:result (link (reverse code) (reverse consts) (reverse regs)))
             ([instr (in-list instrs)])
     (normalize-addrs code consts regs instr))))

(define (add a b c)
  (list 'add a b c))

(define (mul a b c)
  (list 'mul a b c))

(define halt (list 'halt))

;; Running intcode

(define (input->vector input)
  (list->vector
   (map (compose string->number string-trim) (string-split input ","))))

(define (op vec pc fun modes)
  (define a (vector-ref vec (+ pc 1)))
  (define b (vector-ref vec (+ pc 2)))
  (define c (vector-ref vec (+ pc 3)))
  (vector-set! vec c (fun
                      (if (= (first modes) 1)
                          a
                          (vector-ref vec a))
                      (if (= (second modes) 1)
                          b
                          (vector-ref vec b))))
  vec)

(define (input-op vec pc in modes)
  (vector-set! vec (vector-ref vec (add1 pc)) (in))
  vec)

(define (output-op vec pc out modes)
  (define param (vector-ref vec (add1 pc)))
  (if (= (car modes) 1)
      (out param)
      (out (vector-ref vec param)))
  vec)

(define (jump-op vec pc mod modes)
  (define arg1 (let ([tmp (vector-ref vec (+ pc 1))])
                 (if (= (first modes) 0)
                     (vector-ref vec tmp)
                     tmp)))
  (define arg2 (let ([tmp (vector-ref vec (+ pc 2))])
                 (if (= (second modes) 0)
                     (vector-ref vec tmp)
                     tmp)))
  (if (mod (= arg1 0))
      arg2
      (+ pc 3)))

(define (comp-op vec pc pred modes)
  (define arg1 (let ([tmp (vector-ref vec (+ pc 1))])
                 (if (= (first modes) 0)
                     (vector-ref vec tmp)
                     tmp)))
  (define arg2 (let ([tmp (vector-ref vec (+ pc 2))])
                 (if (= (second modes) 0)
                     (vector-ref vec tmp)
                     tmp)))
  (vector-set! vec (vector-ref vec (+ pc 3))
               (if (pred arg1 arg2) 1 0))
  vec)

(module+ test
  (define test-input "1,9,10,3,
              2,3,11,0,
              99,
              30,40,50")
  (define test-vec (input->vector test-input))
  (op test-vec 0 + (list 0 0 0))
  (check-eq? (vector-ref test-vec 3) 70)
  (op test-vec 4 * (list 0 0 0))
  (check-eq? (vector-ref test-vec 0) 3500))

(define (run-prog prog in out)
  (let loop ([pc 0])
    (define-values (opcode modes) (get-modes (vector-ref prog pc) 3))
    (cond
      [(= opcode 99) prog]
      [(= opcode 1) (op prog pc + modes) (loop (+ pc 4))]
      [(= opcode 2) (op prog pc * modes) (loop (+ pc 4))]
      [(= opcode 3) (input-op prog pc in modes) (loop (+ pc 2))]
      [(= opcode 4) (output-op prog pc out modes) (loop (+ pc 2))]
      [(= opcode 5) (loop (jump-op prog pc not modes))]
      [(= opcode 6) (loop (jump-op prog pc values modes))]
      [(= opcode 7) (comp-op prog pc < modes) (loop (+ pc 4))]
      [(= opcode 8) (comp-op prog pc = modes) (loop (+ pc 4))])))

(define (run-asm code in out)
  (run-prog (asm code) in out))

(define (run input in out)
  (define prog (input->vector input))
  (run-prog prog in out))

(module+ test
  (define (in) 1)
  (define (out x) (void))
  (check-equal? (run test-input in out) (vector 3500 9 10 70 2 3 11 0 99 30 40 50))
  (check-equal? (run "1,0,0,0,99" in out) (vector 2 0 0 0 99))
  (check-equal? (run "2, 4, 4, 5, 99, 0" in out) (vector 2 4 4 5 99 9801))
  ;; (* (+ 1 2) (+ 3 4))
  (check-eq? (vector-ref (run-asm
                          (list 
                           (add 1 2 (reg 1))
                           (add 3 4 (reg 2))
                           (mul (reg 1) (reg 2) (reg 3))
                           halt)
                          in out)
                         19)
             21))

(define (get-modes op n)
  (let loop ([op op]
             [pos (expt 10 (add1 n))]
             [modes '()])
    (if (= pos 10)
        (values op modes)
        (let ([mode (quotient op pos)])
          (loop (- op (* mode pos)) (/ pos 10) (cons mode modes))))))

(module+ test
  (define input "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
  (define output #f)
  (define outfun (lambda (x) (set! output x)))
  (define tmp (run input (lambda () 9) (lambda (x) (printf "> ~a~%" x))))
  (check-eq? output 1001)
  (set! tmp (run input (lambda () 8) (lambda (x) (printf "> ~a~%" x))))
  (check-eq? output 1000)
  (set! tmp (run input (lambda () 6) (lambda (x) (printf "> ~a~%" x))))
  (check-eq? output 999))

