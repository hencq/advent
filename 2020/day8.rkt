#lang racket

(define test  (string-split "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" "\n"))

(define (read-prog lines)
  (for/vector ([l (in-list lines)])
    (cons (substring l 0 3) (string->number (substring l 4 (string-length l))))))

(define (run prog)
  (define eof (vector-length prog))
  (let loop ([seen (set)]
             [pc 0]
             [acc 0])
    (cond
      [(set-member? seen pc)
       (values #f acc)]
      [(= pc eof) (values #t acc)]
      [else 
       (let ([instr (vector-ref prog pc)]
             [seen (set-add seen pc)])
         (match instr
           [(cons "acc" n) (loop seen (add1 pc) (+ acc n))]
           [(cons "jmp" n) (loop seen (+ pc n) acc)]
           [(cons "nop" _) (loop seen (add1 pc) acc)]))])))

(run (read-prog test))

(define input (file->lines "input8.txt"))
(run (read-prog input))

(define (toggle prog pc)
  (define copy (vector-copy prog))
  (define instr (vector-ref prog pc))
  (match instr
    [(cons "jmp" n) (vector-set! copy pc (cons "nop" n))]
    [(cons "nop" n) (vector-set! copy pc (cons "jmp" n))])
  copy)

(define (candidates prog)
  (for/list ([i (in-vector prog)]
             [pc (in-naturals)]
             #:when (or (equal? (car i) "jmp")
                        (equal? (car i) "nop")))
    pc))

(candidates (read-prog test))

(define (part2 prog)
  (let loop ([cands (candidates prog)])
    (if (empty? cands)
        #f
        (let ([c (car cands)])
          (define-values (succ? acc) (run (toggle prog c)))
          (if succ?
              (values c acc)
              (loop (cdr cands)))))))

(part2 (read-prog test))
(part2 (read-prog input))
