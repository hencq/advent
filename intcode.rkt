#lang racket

(require rackunit)

(provide run!)

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
n  (append
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

(struct prog (instrs pc done) #:mutable #:transparent)

(define (new-prog instrs)
  (cond
    [(list? instrs) (set! instrs (list->vector instrs))]
    [(string? instrs) (set! instrs (string->ivector instrs))])
  (prog instrs 0 #f))

(define (string->ivector input)
  (list->vector
   (map (compose string->number string-trim) (string-split input ","))))

(define (restart! p)
  (set-prog-pc! p 0)
  (set-prog-done! p #f))

(define (get-cell p pc)
  (vector-ref (prog-instrs p) pc))

(define (get-param p i)
  (vector-ref (prog-instrs p) (+ (prog-pc p) i)))

(define (get-param-val p i)
  (define op (cur-instr p))
  (define base (expt 10 (add1 i)))
  (define mode (quotient (modulo op (* base 10)) base))
  (define param (get-param p i))
  (if (= mode 1)
      param
      (vector-ref (prog-instrs p) param)))

(define (cur-instr p)
  (vector-ref (prog-instrs p) (prog-pc p)))

;; Get short opcode
(define (opcode p)
  (define long-code (cur-instr p))
  (- long-code (* 100 (quotient long-code 100))))

(define (make-math-op fun)
  (lambda (p)
    (define op1 (get-param-val p 1))
    (define op2 (get-param-val p 2))
    (define dest (get-param p 3))
    (vector-set! (prog-instrs p) dest (fun op1 op2))
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
    (define dest (get-param p 3))
    (define result (if (comp a b) 1 0))
    (vector-set! (prog-instrs p) dest result)
    (set-prog-pc! p (+ (prog-pc p) 4))))

(define add! (make-math-op +))
(define mul! (make-math-op *))
(define jiz! (make-jump-op (curry = 0)))
(define jnz! (make-jump-op (compose not (curry = 0))))
(define cmplt! (make-comp-op <))
(define cmpeq! (make-comp-op =))

(define (inp! p in)
  (vector-set! (prog-instrs p) (get-param p 1) (car in))
  (set-prog-pc! p (+ (prog-pc p) 2)))

(define (out! p out)
  (define res (cons (get-param-val p 1) out))
  (set-prog-pc! p (+ (prog-pc p) 2))
  res)

(define (run! prog [input '()])
  (let loop ([in input]
             [out '()])
    (define op (opcode prog))
    (cond
      [(prog-done prog) (void)]
      [(= op 99) (set-prog-done! prog #t) (reverse out)]
      [(= op 1) (add! prog) (loop in out)]
      [(= op 2) (mul! prog) (loop in out)]
      [(= op 3) (inp! prog in) (loop (cdr in) out)]
      [(= op 4) (loop in (out! prog out))]
      [(= op 5) (jnz! prog) (loop in out)]
      [(= op 6) (jiz! prog) (loop in out)]
      [(= op 7) (cmplt! prog) (loop in out)]
      [(= op 8) (cmpeq! prog) (loop in out)])))


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

