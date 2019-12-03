#lang racket
(require rackunit
         threading)

(define input (list
               "x=495, y=2..7"
               "y=7, x=495..501"
               "x=501, y=3..7"
               "x=498, y=2..4"
               "x=506, y=1..2"
               "x=498, y=10..13"
               "x=504, y=10..13"
               "y=13, x=498..504"))

(struct field (x-offset y-offset x-max y-max squares) #:transparent)

(define (field-ref field x y)
  (vector-ref (vector-ref (field-squares field) y)
              (- x (field-x-offset field))))

(define (field-set! field x y val)
  (vector-set! (vector-ref (field-squares field) y)
               (- x (field-x-offset field))
               val))

(define (parse-input lines)
  (define rx (pregexp "(x|y)=(\\d+)(..(\\d+))?"))
  (define clay (apply append 
                      (for/list ([l (in-list lines)])
                        (define coords 
                          (for/hash ([part (string-split l ", ")])
                            (match (regexp-match rx part)
                              [(list _ coord fromstr _ tostr)
                               (define from (string->number fromstr))
                               (define to (if tostr (add1 (string->number tostr)) (add1 from)))
                               (values coord (cons from to))])))
                        (define xs (hash-ref coords "x"))
                        (define ys (hash-ref coords "y"))
                        (for*/list ([x (in-range (car xs) (cdr xs))]
                                    [y (in-range (car ys) (cdr ys))])
                          (cons x y)))))
  (define x-offset (sub1 (car (argmin car clay))))
  (define y-offset (cdr (argmin cdr clay)))
  (define x-max (add1 (car (argmax car clay))))
  (define y-max (cdr (argmax cdr clay)))
  (define squares
    (for/vector ([y (in-range (add1 y-max))])
      (make-vector (add1 (- x-max x-offset)) #\.)))
  (for ([sq (in-list clay)])
    (vector-set! (vector-ref squares (cdr sq)) (- (car sq) x-offset) #\#))
  (vector-set! (vector-ref squares 0) (- 500 x-offset) #\+)
  (field x-offset y-offset x-max y-max squares))

(define (display-field field)
  (for ([row (in-vector (field-squares field))]
        [i (in-naturals)])
    (display (~a i #:min-width 4 #:align 'right))
    (display " ")
    (for ([sq (in-vector row)])
      (display (string sq)))
    (displayln "")))format

(define (flow field x y [dir 0])
  (when (eqv? (field-ref field x y) #\.)
    (field-set! field x y #\|))
  (cond
    [(= y (field-y-max field)) #f]
    [(eqv? (field-ref field x y) #\#) x]
    [(eqv? (field-ref field x (add1 y)) #\.) (flow field x (add1 y) 0)
                                             (flow field x y dir)]
    [(set-member? (set #\# #\~) (field-ref field x (add1 y)))
     (if (zero? dir)
         (let ([left (flow field (sub1 x) y -1)]
               [right (flow field (add1 x) y 1)])
           (when (and (eqv? (field-ref field left y) #\#)
                      (eqv? (field-ref field right y) #\#))
             (for ([x (in-range (add1 left) right)])
               (field-set! field x y #\~)))
           #f)
         (flow field (+ x dir) y dir))]
    [else x]))

(define (count-water field)
  (flow field 500 1)
  (define water (set #\| #\~))
  (define-values (running standing)
    (for*/fold ([running 0]
                [standing 0])
               ([row (in-vector (vector-drop (field-squares field) (field-y-offset field)))]
                [sq (in-vector row)])
      (cond
        [(eqv? sq #\~) (values running (add1 standing))]
        [(eqv? sq #\|) (values (add1 running) standing)]
        [else (values running standing)])))
  (values running standing (+ running standing)))

(module+ test
  (define test (parse-input input))
  (let-values ([(r s t) (count-water test)])
    (check-eq? t 57))

  (define test2 (parse-input (file->lines "day17_input.txt")))
  (let-values ([(r s t) (count-water test2)])
    (check-eq? t 52800)
    (check-eq? s 45210)))

