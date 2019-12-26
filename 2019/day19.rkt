#lang racket

(require "../intcode.rkt")

(define input (file->string "input19.txt"))

(define drone (new-prog input))

(restart! drone)
(run! drone '(1 1))

(define (affected-points drone w h)
  (for*/fold ([sum 0])
             ([x (in-range w)]
              [y (in-range h)])
    (restart! drone)
    (+ sum (car (run! drone (list x y))))))

(affected-points drone 50 50)

(define (display-beam drone w h)
  (for ([y (in-range h)])
    (for ([x (in-range w)])
      (restart! drone)
      (define v (car (run! drone (list x y))))
      (if (= v 1)
          (printf "#")
          (printf ".")))
    (printf "~%")))

(display-beam drone 50 50)

(define (beam-width drone y)
  (define start (let loop ([x 0])
                  (restart! drone)
                  (define pull (car (run! drone (list x y))))
                  (if (= pull 1)
                      x
                      (loop (add1 x)))))
  (define end (let loop ([x start])
                (restart! drone)
                (define pull (car (run! drone (list x y))))
                (if (= pull 0)
                    x
                    (loop (add1 x)))))
  (define width (- end start))
  (values start width))

(define (fits-square? drone x y w h)
  (restart! drone)
  (define top-left (car (run! drone (list x y))))
  (restart! drone)
  (define top-right (car (run! drone (list (sub1 (+ x w)) y))))
  (restart! drone)
  (define bottom-left (car (run! drone (list x (sub1 (+ y h))))))
  (and (= top-left 1) (= top-right 1) (= bottom-left 1)))

(define (find-square-in-row drone y start width target)
  (for/first ([x (in-range start (+ start width))]
              #:when (fits-square? drone x y target target))
    x))

(define (brute-force drone len)
  (let loop ([y 5])
    (define-values (start width) (beam-width drone y))
    (define x (find-square-in-row drone y start width len))
    (if x (list x y (+ (* 10000 x) y)) (loop (add1 y)))))


(find-square-in-row drone 15 11 2)
(beam-width drone 15)
(brute-force drone 3)
(fits-square? drone 12 15 2 2)
