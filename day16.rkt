#lang racket
(require rackunit
         threading)

(define (make-state)
  (make-vector 4))

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

(define (find-matches ops before after operands)
  (match operands
    [(list code A B C)
     (cons code 
           (for/fold ([matches '()])
                     ([(name op) (in-hash ops)])
             (if (equal? after (op before A B C))
                 (cons name matches)
                 matches)))]))

(define (read-input fname)
  (with-input-from-file fname
    (lambda ()
      (define examples
        (let loop ([examples '()])
          (define line (read-line))
          (if (equal? line "")
              (reverse examples)
              (let ([before (list->vector
                             (map string->number
                                  (string-split
                                   (cadr (regexp-match "Before: \\[(.*)]" line)) ", ")))]
                    [op (map string->number (string-split (read-line) " "))]
                    [after (list->vector
                            (map string->number
                                 (string-split
                                  (cadr (regexp-match "After:  \\[(.*)]" (read-line))) ", ")))])
                (read-line)
                (loop (cons (list before op after) examples))))))
      (display (read-line))
      (define program
        (for/list ([_ (in-naturals)])
          (define l (read-line))
          #:break (eof-object? l)
          (map string->number (string-split l " "))))
      (values examples program))))

(define (match-examples ops examples)
  (for/list ([ex (in-list examples)])
    (match ex
      [(list before operands after) 
       (find-matches ops before after operands)])))

(module+ test
  (define-values (examples program) (read-input "day16_input.txt"))
  (define more-than-3 (for/fold ([count 0])
                              ([ex (in-list (match-examples ops examples))])
                      (if (>= (length ex) 4)
                          (add1 count)
                          count)))
  (check-eq? more-than-3 580))

(define (get-code-ops ops examples)
  (define opset (list->set (hash-keys ops)))
  (for/fold ([codes (hash)])
            ([opcode (in-list (match-examples ops examples))])
    (hash-update codes (car opcode) (lambda (o)
                                      (set-intersect (list->set (cdr opcode))
                                                     o))
                 opset)))

(define (propagate ops examples)
  (let loop ([op-codes (get-code-ops ops examples)]
             [found (set)])
    (if (= (set-count found) 16)
        op-codes
        (let-values ([(new-codes new-found)
                      (for/fold ([codes op-codes]
                                 [found found])
                                ([(c ops) (in-hash op-codes)])
                        (if (set? ops)
                            (let ([rem (set-subtract ops found)])
                              (if (= 1 (set-count rem))
                                  (values (hash-set codes c (set-first rem))
                                          (set-add found (set-first rem)))
                                  (values (hash-set codes c rem)
                                          found)))
                            (values codes found)))])
          (loop new-codes new-found)))))

(define (run ops codes program)
  (define table (for/hash ([(c op) (in-hash codes)])
                  (values c (hash-ref ops op))))
  (for/fold ([regs (vector 0 0 0 0)])
            ([instr (in-list program)])
    (define op (hash-ref table (car instr)))
    (apply op regs (cdr instr))))

(module+ test
  (define codes (propagate ops examples))
  (define result (run ops codes program))
  (check-eq? (vector-ref result 0) 537))
