#lang racket


(struct instr (op A B C) #:transparent)
(struct state (ops instrs regs ipreg ip) #:transparent)

(define (make-state ops instrs ipreg [regcount 6])
  (state ops instrs (make-vector regcount 0) ipreg 0))

(define (make-op op type1 type2)
  (lambda (orig-regs A B C)
    (define regs (vector-copy orig-regs))
    (define oper1 (cond
                    [(eq? type1 'i) A]
                    [(eq? type1 'r) (vector-ref regs A)]))
    (define oper2 (cond
                    [(eq? type2 'i) B]
                    [(eq? type2 'r) (vector-ref regs B)]))
    (vector-set! regs C (op oper1 oper2))
    regs))

(define ops (hash 
             'addr (make-op + 'r 'r)
             'addi (make-op + 'r 'i)
             'mulr (make-op * 'r 'r)
             'muli (make-op * 'r 'i)

             'banr (make-op bitwise-and 'r 'r)
             'bani (make-op bitwise-and 'r 'i)
             'borr (make-op bitwise-ior 'r 'r)
             'bori (make-op bitwise-ior 'r 'i)

             'setr (make-op (lambda (a _) a) 'r 'i)
             'seti (make-op (lambda (a _) a) 'i 'i)

             'gtir (make-op (lambda (a b) (if (> a b) 1 0)) 'i 'r)
             'gtri (make-op (lambda (a b) (if (> a b) 1 0)) 'r 'i)
             'gtrr (make-op (lambda (a b) (if (> a b) 1 0)) 'r 'r)

             'eqir (make-op (lambda (a b) (if (= a b) 1 0)) 'i 'r)
             'eqri (make-op (lambda (a b) (if (= a b) 1 0)) 'r 'i)
             'eqrr (make-op (lambda (a b) (if (= a b) 1 0)) 'r 'r)))

(define (instr->asm ip addr i)
  (match i
    [(instr 'addr A B C) #:when (and (= C ip) (= A ip)) (~a "goto r" B " + " (add1 addr))]
    [(instr 'addi A B C) #:when (and (= C ip) (= A ip)) (~a "goto " (+ addr B 1))]
    [(instr 'addr A B C) #:when (and (= C ip) (= B ip)) (~a "goto r" A " + " (add1 addr))]
    [(instr 'addi A B C) #:when (and (= C ip) (= B ip)) (~a "goto " (+ addr A 1))]
    [(instr 'setr A B C) #:when (and (= C ip) (= A ip)) (~a "goto " (add1 addr))]    
    
    [(instr 'addr A B C) #:when (= C ip) (~a "goto r" A " + r" B " + 1")]
    [(instr 'addi A B C) #:when (= C ip) (~a "goto r" A " + " B " + 1")]
    [(instr 'setr A B C) #:when (= C ip) (~a "goto r" A " + 1")]
    [(instr 'seti A B C) #:when (= C ip) (~a "goto " (add1 A))]

    
    [(instr 'addr A B C) (~a "r" C " = r" A " + r" B)]
    [(instr 'addi A B C) (~a "r" C " = r" A " + " B)]
    [(instr 'mulr A B C) (~a "r" C " = r" A " * r" B)]
    [(instr 'muli A B C) (~a "r" C " = r" A " * " B)]

    [(instr 'banr A B C) (~a "r" C " = r" A " & r" B)]
    [(instr 'bani A B C) (~a "r" C " = r" A " & " B)]
    [(instr 'borr A B C) (~a "r" C " = r" A " | r" B)]
    [(instr 'bori A B C) (~a "r" C " = r" A " | " B)]

    [(instr 'setr A B C) (~a "r" C " = r" A)]
    [(instr 'seti A B C) (~a "r" C " = " A)]

    [(instr 'gtir A B C) (~a "if " A " > r" B " then r" C " = " 1 " else r" C " = 0")]
    [(instr 'gtri A B C) (~a "if r" A " > " B " then r" C " = " 1 " else r" C " = 0")]
    [(instr 'gtrr A B C) (~a "if r" A " > r" B " then r" C " = " 1 " else r" C " = 0")]

    [(instr 'eqir A B C) (~a "if " A " = r" B " then r" C " = " 1 " else r" C " = 0")]
    [(instr 'eqri A B C) (~a "if r" A " = " B " then r" C " = " 1 " else r" C " = 0")]
    [(instr 'eqrr A B C) (~a "if r" A " = r" B " then r" C " = " 1 " else r" C " = 0")]))

(define (program->asm state)
  (for/list ([i (in-vector (state-instrs state))]
             [addr (in-naturals)])
    (~a (~a addr) ":" #\newline
        (instr->asm (state-ipreg state) addr i))))

;; (with-output-to-file "day19_asm2"
;;   (lambda ()
;;     (for ([l (in-list (program->asm input))])
;;       (displayln l))))

(define (parse-input lines)
  (define ip (string->number (cadr (regexp-match "#ip (.*)"(car lines)))))
  (define instrs (for/vector ([l (in-list (cdr lines))])
                   (define parts (regexp-match "(.*) (.*) (.*) (.*)" l))
                   (apply instr (string->symbol (cadr parts))
                          (map string->number (cddr parts)))))
  (make-state ops instrs ip))


(define test (parse-input (list
                           "#ip 0"
                           "seti 5 0 1"
                           "seti 6 0 2"
                           "addi 0 1 0"
                           "addr 1 2 3"
                           "setr 1 0 0"
                           "seti 8 0 4"
                           "seti 9 0 5")))

(define (step st [show #f])
  (if (and (>= (state-ip st) 0) (< (state-ip st) (vector-length (state-instrs st))))
      (let ([regs (vector-copy (state-regs st))])
        (vector-set! regs (state-ipreg st) (state-ip st))
        (let* ([instr (vector-ref (state-instrs st) (state-ip st))]
               [op (hash-ref (state-ops st) (instr-op instr))]
               [new-regs (op regs (instr-A instr) (instr-B instr) (instr-C instr))]
               [new-ip (add1 (vector-ref new-regs (state-ipreg st)))])
          (when show
            (printf "ip=~a ~a ~a ~a~%" (~a (state-ip st) #:min-width 2) (state-regs st) instr new-regs))
          (struct-copy state st [ip new-ip] [regs new-regs])))
      #f))

(define (run-program state #:show [show #f] #:max [max +inf.0])
  (let loop ([state state]
             [i 0])
    (define next (step state show))
    (if (and next (< i max))
        (loop next (add1 i))
        state)))

(define input (parse-input (file->lines "day19_input.txt")))

(define (program r0 r1 r2 r3 r4 r5)
  (set! r5 (+ r5 2))
  (set! r5 (* r5 r5))
  (set! r5 (* 19 r5))
  (set! r5 (* 11 r5))
  (set! r3 (add1 r3))
  (set! r3 (* r3 22))
  (set! r3 (+ 18 r3))
  (set! r5 (+ r5 r3))
  (when (= r0 1)
    (set! r3 27)
    (set! r3 (* r3 28))
    (set! r3 (+ 29 r3))
    (set! r3 (* 30 r3))
    (set! r3 (* r3 14))
    (set! r3 (* r3 32))
    (set! r5 (+ r5 r3))
    (set! r0 0))
  (set! r1 1)
  (printf "Before outer loop: ~a, ~a, ~a, ~a, ~a, ~a~%" r0 r1 r2 r3 r4 r5)
  (let outer ()
    (set! r2 1)
    ;; (let inner ()
    ;;   (set! r3 (* r1 r2))
    ;;   (when (= r3 r5)
    ;;     (set! r0 (+ r1 r0)))
    ;;   (set! r2 (add1 r2))
    ;;   (when (<= r2 r5)
    ;;     (inner)))
    ;; (set! r1 (add1 r1))
    (when (= (remainder r5 r1) 0)
      (set! r0 (+ r0 r1)))
    (set! r1 (add1 r1))
    (if (<= r1 r5)
        (outer)
        r0)))
