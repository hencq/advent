#lang racket

(require "../intcode.rkt")

(define (paint input color)
  (define code (new-prog input))
  (let loop ([area (hash 0 color)]
             [pos 0]
             [dir +i])
    (if (prog-done code)
        area
        (let* ([col (hash-ref area pos 0)]
               [out (run! code (list col))]
               [newcol (car out)]
               [newdir (if (= (cadr out) 0)
                           (* dir +i)
                           (* dir -i))]
               [newpos (+ pos newdir)])
          (loop (hash-set area pos newcol)
                newpos
                newdir)))))

(define input (file->string "input11.txt"))
(hash-count (paint input 0))

(define area (paint input 1))

(define (dimensions area)
  (define ys (map imag-part (hash-keys area)))
  (define xs (map real-part (hash-keys area)))
  (list
   (apply min xs)
   (apply min ys)
   (apply max xs)
   (apply max ys)))

(define (display-area area)
  (match-define (list x1 y1 x2 y2) (dimensions area))
  (for ([y (in-range y2 (sub1 y1) -1)])
    (for ([x (in-range x1 (add1 x2))])
      (define pos (make-rectangular x y))
      (printf "~a" (if (= (hash-ref area pos 0) 0) #\space #\#)))
    (printf "~%")))

(display-area area)
