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
  (vector-ref (vector-ref (field-squares field) (- y (field-y-offset field)))
              (- x (field-x-offset field))))

(define (field-set! field x y val)
  (vector-set! (vector-ref (field-squares field) (- y (field-y-offset field)))
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
  (define x-offset (car (argmin car clay)))
  (define y-offset (min 0 (cdr (argmin cdr clay))))
  (define x-max (car (argmax car clay)))
  (define y-max (cdr (argmax cdr clay)))
  (define squares
    (for/vector ([y (in-range (add1 (- y-max y-offset)))])
      (make-vector (add1 (- x-max x-offset)) #\.)))
  (for ([sq (in-list clay)])
    (vector-set! (vector-ref squares (- (cdr sq) y-offset)) (- (car sq) x-offset) #\#))
  (vector-set! (vector-ref squares (- 0 y-offset)) (- 500 x-offset) #\+)
  (field x-offset y-offset x-max y-max squares))

(define (wall-to-wall? field x y)
  (displayln "In wall to wall")
  (and
    (let left ([x x])
      (cond
        [(<= x (field-x-offset field)) #f]
        [(>= y (field-y-max field)) #f]
        [(eqv? (field-ref field x (add1 y)) #\.) #f]
        [(eqv? (field-ref field (sub1 x) y) #\#)  #t]
        [(eqv? (field-ref field (sub1 x) y) #\~)  #t]
        [else (left (sub1 x))]))
    (let right ([x x])
      (cond
        [(>= x (field-x-max field))  #f]
        [(>= y (field-y-max field))  #f]
        [(eqv? (field-ref field x (add1 y)) #\.)  #f]
        [(eqv? (field-ref field (add1 x) y) #\#)  #t]
        [(eqv? (field-ref field (add1 x) y) #\~)  #t]
        [else (right (add1 x))]))))

(define (dfs! field x y)
  (printf "~a, ~a~%" x y)
  (display-field field)
  (define res
    (cond
      ;; Is the current square solid?
      [(< x (field-x-offset field)) #f]
      [(> x (field-x-max field)) #f]
      [(< y (field-y-offset field)) #f]
      [(> y (field-y-max field)) (displayln "Off screen!") #f]
      [(eqv? (field-ref field x y) #\#) #t]
      [(eqv? (field-ref field x y) #\~) #t]
      [else (field-set! field x y #\|)
            (if (dfs! field x (add1 y))
                (let ()
                  ;; (when (wall-to-wall? field x y)
                  ;;   (field-set! field x y #\~))
                  (dfs! field (sub1 x) y)
                  ;; (dfs! field (add1 x) y)
                  #t)
                #f)]))
  res)

(define (display-field field)
  (for ([row (in-vector (field-squares field))]
        [i (in-naturals)])
    (display (~a i #:min-width 2 #:align 'right))
    (display " ")
    (for ([sq (in-vector row)])
      (display (string sq)))
    (displayln "")))format

(define test (parse-input input))

(define (push-water field x y [dir 'left])
  (printf "Pushing ~a,~a~%" x y)
  (cond
    [(< x (field-x-offset field)) #t]
    [(> x (field-x-max field)) #t]
    [(< y (field-y-offset field)) #t]
    [(> y (field-y-max field)) (displayln "Off screen!") #t]
    [(eqv? (field-ref field x y) #\#) #f]
    [(and (eqv? dir 'left)
          (eqv? (field-ref field x y) #\~)) (or (push-water field (sub1 x) y 'left)
                                                (push-water field (add1 x) y 'right))]
    [(eqv? (field-ref field x y) #\~) (push-water field (add1 x) y 'right)]
    [(push-water field x (add1 y)) (field-set! field x y #\|) #t]
    [else (field-set! field x y #\~) #t]))

(for ([i (in-range 5)])
  (push-water test 500 1)
  (display-field test))
