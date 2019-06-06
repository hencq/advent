#lang racket
(require rackunit
         threading)

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
    ;; [(instr 'addr A B C) #:when (and (= C ip) (= A ip)) (~a "goto r" B " + " (add1 addr))]
    ;; [(instr 'addi A B C) #:when (and (= C ip) (= A ip)) (~a "goto " (+ addr B 1))]
    ;; [(instr 'addr A B C) #:when (and (= C ip) (= B ip)) (~a "goto r" A " + " (add1 addr))]
    ;; [(instr 'addi A B C) #:when (and (= C ip) (= B ip)) (~a "goto " (+ addr A 1))]
    ;; [(instr 'setr A B C) #:when (and (= C ip) (= A ip)) (~a "goto " (add1 addr))]    
    
    ;; [(instr 'addr A B C) #:when (= C ip) (~a "goto r" A " + r" B " + 1")]
    ;; [(instr 'addi A B C) #:when (= C ip) (~a "goto r" A " + " B " + 1")]
    ;; [(instr 'setr A B C) #:when (= C ip) (~a "goto r" A " + 1")]
    ;; [(instr 'seti A B C) #:when (= C ip) (~a "goto " (add1 A))]

    
    [(instr 'addr A B C) `(set! (reg ,C) (+ (reg ,A) (reg ,B)))]
    [(instr 'addi A B C) `(set! (reg ,C) (+ (reg ,A) ,B))]
    [(instr 'mulr A B C) `(set! (reg ,C) (* (reg ,A) (reg ,B)))]
    [(instr 'muli A B C) `(set! (reg ,C) (* (reg ,A) ,B))]

    [(instr 'banr A B C) `(set! (reg ,C) (bitwise-and (reg ,A) (reg ,B)))]
    [(instr 'bani A B C) `(set! (reg ,C) (bitwise-and (reg ,A) ,B))]
    [(instr 'borr A B C) `(set! (reg ,C) (bitwise-ior (reg ,A) (reg ,B)))]
    [(instr 'bori A B C) `(set! (reg ,C) (bitwise-ior (reg ,A) ,B))]

    [(instr 'setr A B C) `(set! (reg ,C) (reg ,A))]
    [(instr 'seti A B C) `(set! (reg ,C) ,A)]

    [(instr 'gtir A B C) `(set! (reg ,C) (if (> ,A (reg ,B)) 1 0))]
    [(instr 'gtri A B C) `(set! (reg ,C) (if (> (reg ,A) ,B) 1 0))]
    [(instr 'gtrr A B C) `(set! (reg ,C) (if (> (reg ,A) (reg ,B)) 1 0))]

    [(instr 'eqir A B C) `(set! (reg ,C) (if (= ,A (reg ,B)) 1 0))]
    [(instr 'eqri A B C) `(set! (reg ,C) (if (= (reg ,A) ,B) 1 0))]
    [(instr 'eqrr A B C) `(set! (reg ,C) (if (= (reg ,A) (reg ,B)) 1 0))]))

(define (replace-ipreg instr ip addr)
  (define (simplify op)
    (if (cons? op)
        (if (equal? op `(reg ,ip))
            (add1 addr)
            (map simplify op))
        op))
  (match instr
    [`(set! (reg ,r) ,val) #:when (and (= r ip) (number? val)) `(goto ,(add1 val))]
    [`(set! (reg ,r) ,val) #:when (= r ip) `(goto ,(simplify val))]
    [default default]))

(define (program->asm state)
  (~>
   (for/list ([i (in-vector (state-instrs state))]
              [addr (in-naturals)])
     (~> 
      (instr->asm (state-ipreg state) addr i)
      (replace-ipreg (state-ipreg state) addr)
      (simplify-goto)))
   (if->when)
   (counter->label)
   (remove-noops)))

(define (compile state)
  (list->vector
   (map elf->bcode
        (~>
         (for/list ([i (in-vector (state-instrs state))]
                    [addr (in-naturals)])
           (~> 
            (instr->asm (state-ipreg state) addr i)
            (replace-ipreg (state-ipreg state) addr)
            (simplify-goto)))
         (if->when)
         (compress-noops)))))

(define (compress-noops program)
  (define noops 
    (for/list ([instr (in-list program)]
               [i (in-naturals)]
               #:when (equal? instr '(noop)))
      i))
  (define (noops-before x)
    (length (filter (curry > x) noops)))
  (for/list ([instr (in-list program)]
             #:unless (equal? instr '(noop)))
    (match instr
      [`(goto ,x) `(goto ,(- x (noops-before x)))]
      [`(when ,pred (goto ,x)) `(when ,pred (goto ,(- x (noops-before x))))]
      [instr instr])))

(define (elf->bcode instr)
  (match instr
    [`(set! (reg ,a) ,b) #:when (number? b) `(seti ,a ,b)]
    [`(set! (reg ,a) (reg ,b)) `(setr ,a ,b)]
    [`(set! (reg ,a) (bitwise-and (reg ,b) ,c)) #:when (number? c) `(andi ,a ,b ,c)]
    [`(set! (reg ,a) (bitwise-ior (reg ,b) ,c)) #:when (number? c) `(ori ,a ,b ,c)]
    [`(when (= (reg ,a) ,b) (goto ,c)) #:when (number? b) `(eqri ,a ,b ,c)]
    [`(when (= (reg ,a) (reg ,b)) (goto ,c))  `(eqrr ,a ,b ,c)]
    [`(when (> ,a (reg ,b)) (goto ,c)) #:when (number? a) `(gtir ,a ,b ,c)]
    [`(when (> (reg ,a) (reg ,b)) (goto ,c)) `(gtrr ,a ,b ,c)]
    [`(set! (reg ,a) (+ (reg ,b) (reg ,c))) `(addr ,a ,b ,c)]
    [`(set! (reg ,a) (+ (reg ,b) ,c)) #:when (number? c) `(addi ,a ,b ,c)]
    [`(set! (reg ,a) (* (reg ,b) (reg ,c))) `(mulr ,a ,b ,c)]
    [`(set! (reg ,a) (* (reg ,b) ,c)) #:when (number? c) `(muli ,a ,b ,c)]
    [instr instr]))

(define (interpret-bcode program [r0 0])
  (define len (vector-length program))
  (define regs (vector r0 0 0 0 0 0))
  (define seen (mutable-set))
  (define last #f)
  (let loop ([ip 0])
    (cond
      [(>= ip len) regs]
      [(= ip 7) (super-instr regs) (loop 12)]
      [(= ip 25) (let ([r2 (vector-ref regs 2)])
                   (if (set-member? seen r2)
                       last
                       (let ()
                         (set! last r2)
                         (set-add! seen r2)
                         (if (= r2 (vector-ref regs 0))
                             regs
                             (loop 5)))))]
      [else (let ([instr (vector-ref program ip)])
              (match instr
                [`(seti ,a ,b) (vector-set! regs a b) (loop (add1 ip))]
                [`(setr ,a ,b) (vector-set! regs a (vector-ref regs b)) (loop (add1 ip))]
                [`(andi ,a ,b ,c) (vector-set! regs a (bitwise-and (vector-ref regs b) c))
                                  (loop (add1 ip))]
                [`(ori ,a ,b ,c) (vector-set! regs a (bitwise-ior (vector-ref regs b) c))
                                 (loop (add1 ip))]
                [`(eqri ,a ,b ,c) (if (= (vector-ref regs a) b)
                                      (loop c)
                                      (loop (add1 ip)))]
                [`(eqrr ,a ,b ,c) (if (= (vector-ref regs a) (vector-ref regs b))
                                      (loop c)
                                      (loop (add1 ip)))]
                [`(gtir ,a ,b ,c) (if (> a (vector-ref regs b))
                                      (loop c)
                                      (loop (add1 ip)))]
                [`(gtrr ,a ,b ,c) (if (> (vector-ref regs a) (vector-ref regs b))
                                      (loop c)
                                      (loop (add1 ip)))]
                [`(addr ,a ,b ,c) (vector-set! regs a (+ 
                                                       (vector-ref regs b)
                                                       (vector-ref regs c))) (loop (add1 ip))]
                [`(addi ,a ,b ,c) (vector-set! regs a (+ (vector-ref regs b) c)) (loop (add1 ip))]
                [`(mulr ,a ,b ,c) (vector-set! regs a (*
                                                       (vector-ref regs b)
                                                       (vector-ref regs c))) (loop (add1 ip))]
                [`(muli ,a ,b ,c) (vector-set! regs a (* (vector-ref regs b) c)) (loop (add1 ip))]
                [`(goto ,a) (loop a)]))])))

(define (super-instr regs)
  (define r3 (bitwise-and (vector-ref regs 5) 255))
  (define r2 (+ (vector-ref regs 2) r3))
  (define r2* (bitwise-and r2 16777215))
  (define r2** (* r2* 65899))
  (vector-set! regs 2 (bitwise-and r2** 16777215)))

(define (if->when program)
  (let loop ([todo program]
             [done '()])
    (cond
      [(empty? todo) (reverse done)]
      [(= (length todo) 1) (reverse (cons (car todo) done))]
      [else 
       (let ([i1 (car todo)]
             [i2 (cadr todo)])
         (match i1
           [`(set! (reg ,r1) (if ,pred 1 0)) (match i2
                                               [`(goto (+ (reg ,r2) ,x))
                                                #:when (= r1 r2)
                                                (loop (cddr todo)
                                                      (cons '(noop)
                                                            (cons `(when ,pred
                                                                     (goto ,(add1 x))) done)))]
                                               [else (loop (cddr todo)
                                                           (cons i2 (cons i1 done)))])]
           [else (loop (cdr todo) (cons i1 done))]))])))

(define (simplify-goto instr)
  (match instr
    [`(goto (+ ,a ,b)) #:when (and (number? a) (number? b)) `(goto ,(+ a b))]
    [instr instr]))

(define (remove-noops program)
  (filter (lambda (i)
            (not (equal? i '(noop))))
          program))

(define (counter->label program)
  (define locs 
    (for/fold ([locs (set)])
              ([instr (in-list program)])
      (match instr
        [`(goto ,a) #:when (number? a) (set-add locs a)]
        [`(when ,pred (goto ,a)) #:when (number? a) (set-add locs a)]
        [else locs])))
  (for/fold ([labeled '()]
             #:result (reverse labeled))
            ([instr (in-list program)]
             [i (in-naturals)])
    (if (set-member? locs i)
        (cons instr 
              (cons `(label ,i) labeled))
        (cons instr labeled))))


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

(define (run-program state #:show [show #f] #:max [maxi +inf.0])
  (define seen (mutable-set))
  (let loop ([state state]
             [i 0])
    (when (= (state-ip state) 28)
      (define r2 (vector-ref (state-regs state) 2))
      (if (set-member? seen r2)
          (printf "Seen ~a before at ~a~%" r2 i)
          (set-add! seen r2))
      ;; (printf "Exit test: ~a~%" (state-regs state))
      )
    (define next (step state show))
    (if (and next (< i maxi))
        (loop next (add1 i))
        ;; state
        (set-count seen)
        )))

(define input (parse-input (file->lines "day21_input.txt")))


