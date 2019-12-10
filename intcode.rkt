#lang racket

(require rackunit
         (for-syntax syntax/parse))

(provide new-prog
         run!
         restart!
         set-prog-pc!
         prog-done)

;; Some intcode utilities to (hopefully) help with later puzzles

;; A little mini assembler to help test things

(struct var (name) #:transparent)
(struct ptr (addr) #:transparent)
(struct rel (addr) #:transparent)
(struct ref (name) #:transparent)

(define (make-op code params)
  (for/fold ([op code]
             [dec 100]
             [ipars '()]
             #:result (cons op (reverse ipars)))
            ([p (in-list params)])
    (cond
      [(number? p) (values (+ op dec) (* 10 dec) (cons p ipars))]
      [(ptr? p) (values op (* 10 dec) (cons (ptr-addr p) ipars))]
      [(rel? p) (values (+ op (* 2 dec)) (* 10 dec) (cons (rel-addr p) ipars))]
      [(var? p) (values op (* 10 dec) (cons p ipars))]
      [(ref? p) (values op (* 10 dec) (cons p ipars))])))

(module+ test
  (check-equal? (make-op 2 (list (rel 1) 2 (ptr 10))) '(1202 1 2 10))
  (check-equal? (make-op 4 (list (ptr 21))) '(4 21))
  (check-equal? (make-op 5 (list (ptr 10) 12 (ref 'A))) (list 1005 10 12 (ref 'A))))

(define (ops->int instrs)
  (define codes (hash 'add 1
                      'mul 2
                      'inp 3
                      'out 4
                      'jnz 5
                      'jiz 6
                      'cmp< 7
                      'cmp= 8
                      'arb 9
                      'halt 99))
  (append*
   (for/list ([i (in-list instrs)])
     (define op (car i))
     (cond
       [(hash-has-key? codes op) (make-op (hash-ref codes op) (cdr i))]
       [(equal? op 'label) (list i)]))))

(define (label? i)
  (and (list? i)
       (equal? (car i) 'label)))

(define (replace-labels code)
  (define addrs (for/fold ([addrs (hash)]
                           [c 0]
                           #:result addrs)
                          ([ins (in-list code)])
                  (if (label? ins)
                      (values (hash-set addrs (cadr ins) c) c)
                      (values addrs (add1 c)))))
  (for/list ([ins (in-list code)]
             #:when (not (label? ins)))
    (if (ref? ins)
        (hash-ref addrs (ref-name ins))
        ins)))

(define (replace-vars code)
  (for/fold ([vars (hash)]
             [acc '()]
             [counter (length code)]
             #:result (append
                       (reverse acc)
                       (make-list (hash-count vars) 0)))
            ([i (in-list code)])
    (cond
      [(and (var? i) (hash-has-key? vars i)) (values vars
                                                     (cons (hash-ref vars i) acc)
                                                     counter)]
      [(var? i) (values (hash-set vars i counter)
                        (cons counter acc)
                        (add1 counter))]
      [else (values vars (cons i acc) counter)])))

(define compile (compose replace-vars
                         replace-labels
                         ops->int))


(define-syntax (asm stx)
  (syntax-parse stx
    [(_ (op arg ...) ...) #'(list
                             (list (quote op) arg ...) ...)]))


(module+ test
  (check-equal? 
   (compile
    (asm
     (add 1 2 (var 'a))
     (add 3 4 (var 'b))
     (label 'A)
     (mul (var 'a) (var 'b) (var 'c))
     (out (var 'c))
     (label 'end)
     (halt)))
   '(1101 1 2 15 1101 3 4 16 2 15 16 17 4 17 99 0 0 0)))





;; Running intcode

(struct prog (uninit instrs pc rb done) #:mutable #:transparent)

(define (new-prog instrs)
  (cond
    [(list? instrs) (set! instrs (list->vector instrs))]
    [(string? instrs) (set! instrs (string->ivector instrs))])
  (define size (expt 2 (+ 2
                          (inexact->exact (floor (log (vector-length instrs) 2))))))
  (define prog-vec (make-vector size))
  (vector-copy! prog-vec 0 instrs)
  (prog instrs prog-vec 0 0 #f))

(define (string->ivector input)
  (list->vector
   (map (compose string->number string-trim) (string-split input ","))))

(define (double-memory! p)
  (define cur-size (vector-length (prog-instrs p)))
  (define new-vec (make-vector (* 2 cur-size)))
  (vector-copy! new-vec 0 (prog-instrs p))
  (set-prog-instrs! p new-vec))

(define (restart! p)
  (set-prog-pc! p 0)
  (set-prog-rb! p 0)
  (vector-fill! (prog-instrs p) 0)
  (vector-copy! (prog-instrs p) 0 (prog-uninit p))
  (set-prog-done! p #f))

(define (get-cell p i)
  (when (>= i (vector-length (prog-instrs p)))
    (double-memory! p)
    (get-cell p i))
  (vector-ref (prog-instrs p) i))

(define (set-cell! p i v)
  (when (>= i (vector-length (prog-instrs p)))
    (double-memory! p)
    (set-cell! p i v))
  (vector-set! (prog-instrs p) i v))

(define (get-param p i)
  (get-cell p (+ (prog-pc p) i)))

(define (get-param-val p i)
  (define op (cur-instr p))
  (define base (expt 10 (add1 i)))
  (define mode (quotient (modulo op (* base 10)) base))
  (define param (get-param p i))
  (cond
    [(= mode 0) (get-cell p param)]
    [(= mode 1) param]
    [(= mode 2) (get-cell p (+ (prog-rb p) param))]))

(define (set-param! p i v)
  (define op (cur-instr p))
  (define base (expt 10 (add1 i)))
  (define mode (quotient (modulo op (* base 10)) base))
  (define param (get-param p i))
  (cond
    [(= mode 0) (set-cell! p param v)]
    [(= mode 1) (raise "Writing to direct param")]
    [(= mode 2) (set-cell! p (+ (prog-rb p) param) v)]))

(define (cur-instr p)
  (get-cell p (prog-pc p)))

;; Get short opcode
(define (opcode p)
  (define long-code (cur-instr p))
  (- long-code (* 100 (quotient long-code 100))))

(define (make-math-op fun)
  (lambda (p)
    (define op1 (get-param-val p 1))
    (define op2 (get-param-val p 2))
    (define dest (get-param p 3))
    (set-param! p 3 (fun op1 op2))
    (set-prog-pc! p (+ (prog-pc p) 4))))

(define (make-jump-op pred)
  (lambda (p)
    (define x (get-param-val p 1))
    (define jmp (get-param-val p 2))
    (if (pred x)
        (set-prog-pc! p jmp)
        (set-prog-pc! p (+ (prog-pc p) 3)))))

(define (make-comp-op comp)
  (lambda (p)
    (define a (get-param-val p 1))
    (define b (get-param-val p 2))
    (define result (if (comp a b) 1 0))
    (set-param! p 3 result)
    (set-prog-pc! p (+ (prog-pc p) 4))))

(define add! (make-math-op +))
(define mul! (make-math-op *))
(define jiz! (make-jump-op (curry = 0)))
(define jnz! (make-jump-op (compose not (curry = 0))))
(define cmp<! (make-comp-op <))
(define cmp=! (make-comp-op =))

(define (inp! p in)
  (set-param! p 1 (car in))
  (set-prog-pc! p (+ (prog-pc p) 2))
  (cdr in))

(define (out! p out)
  (define res (cons (get-param-val p 1) out))
  (set-prog-pc! p (+ (prog-pc p) 2))
  res)

(define (arb! p)
  (define delta (get-param-val p 1))
  (set-prog-rb! p (+ (prog-rb p) delta))
  (set-prog-pc! p (+ (prog-pc p) 2)))

(define (run! prog [input '()] #:debug [debug #f])
  (let loop ([in input]
             [out '()])
    (define op (opcode prog))
    (when debug
      (printf ">~a (~a)~%" op in))
    (cond
      [(prog-done prog) (void)]
      [(= op 99) (set-prog-done! prog #t) (reverse out)]
      [(= op 1) (add! prog) (loop in out)]
      [(= op 2) (mul! prog) (loop in out)]
      [(= op 3) (if (empty? in)
                    (reverse out)
                    (loop (inp! prog in) out))]
      [(= op 4) (loop in (out! prog out))]
      [(= op 5) (jnz! prog) (loop in out)]
      [(= op 6) (jiz! prog) (loop in out)]
      [(= op 7) (cmp<! prog) (loop in out)]
      [(= op 8) (cmp=! prog) (loop in out)]
      [(= op 9) (arb! prog) (loop in out)])))


(module+ test
  (define input "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
  (define p (new-prog input))
  (check-eq? (car (run! p (list 9))) 1001)
  (restart! p)
  (check-eq? (car (run! p (list 8))) 1000)
  (restart! p)
  (check-eq? (car (run! p (list 7))) 999))

