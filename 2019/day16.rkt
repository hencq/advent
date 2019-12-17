#lang racket

(require "../utils.rkt")

(define test "12345678")

(define (digits str)
  (filter values (map string->number (string-split str ""))))

(define base-pattern (list 0 1 0 -1))

(define (pattern step size)
  (define pat (append* 
               (for/list ([p (in-list base-pattern)])
                 (make-list step p))))
  (cdr 
   (for/list ([_ (in-range (add1 size))]
              [p (in-cycle pat)])
     p)))

(define (calc-element xs pat)
  (for/fold ([sum 0])
            ([p (in-list pat)]
             [x (in-list xs)])
    (+ sum (* p x))))

(define (calc-phase xs phase [offset 0])
  (define pats (for/list ([_ (in-list xs)]
                          [i (in-naturals 1)])
                 (drop (pattern i (length xs)) offset)))
  (for/fold ([xs xs])
            ([i (in-range phase)])
    (for/list ([p (in-list pats)])
      (modulo (abs (calc-element xs p)) 10))))

(define (phase1 input)
  (define xs (digits input))
  (take (calc-phase xs 100) 8))

(phase1 "80871224585914546619083218645595")
(phase1 (file->string "input16.txt"))

(define (repeated-pattern step size repeat)
  (define total-size (* size repeat))
  (define pat (pattern step total-size))
  (partition pat size))

(define (repeated-calc-phase xs phases repeat)
  (define pats (for/list ([_ (in-list xs)]
                          [i (in-naturals 1)])
                 (repeated-pattern i (length xs) repeat)))
  (define cache (make-hash))
  (for/fold ([xs xs])
            ([i (in-range phases)])
    (for*/list ([rep (in-list pats)]
                [p (in-list rep)])
      (define key (cons xs p))
      (if (hash-has-key? cache key)
          (hash-ref cache key)
          (let ([elt (modulo (abs (calc-element xs p)) 10)])
            (hash-set! cache key elt)
            elt))))
  (hash-count cache))

(define (offset digits)
  (for/fold ([sum 0]
             [place 1]
             #:result sum)
            ([d (in-vector digits 6 -1 -1)])
    (values
     (+ sum (* d place))
     (* 10 place))))


(define (offset-vector input)
  (define src (list->vector (digits input)))
  (define off (offset src))
  (define srclen (vector-length src))
  (define destlen (- (* 10000 srclen) off))
  (define dest (make-vector destlen))
  (define rem (modulo off srclen))
  (vector-copy! dest 0 src rem)
  (for ([i (in-range (- srclen rem) destlen srclen)])
    (vector-copy! dest i src))
  dest)


(define (phase2 input)
  (define vec (offset-vector input))
  (for ([i (in-range 100)])
    (for/fold  ([sum 0])
               ([i (in-range (vector-length vec) 0 -1)])
      (let ([sum (+ sum (vector-ref vec (sub1 i)))])
        (vector-set! vec (sub1 i) (modulo sum 10))
        sum)))
  (vector-take vec 8))

(phase2 "03036732577212944063491565474664")
(phase2 "02935109699940807407585447034323")
(time (phase2 (file->string "input16.txt")))
















