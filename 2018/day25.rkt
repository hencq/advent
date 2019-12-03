#lang racket

(define (parse lines)
  (define (parse-line line)
    (map string->number (string-split line ",")))
  (map parse-line lines))

(define test0 (parse (list
                      "0,0,0,0"
                      "3,0,0,0"
                      "0,3,0,0"
                      "0,0,3,0"
                      "0,0,0,3"
                      "0,0,0,6"
                      "9,0,0,0"
                      "12,0,0,0")))

(define test1 (parse (list
                     "-1,2,2,0"
                     "0,0,2,-2"
                     "0,0,0,-2"
                     "-1,2,0,0"
                     "-2,-2,-2,2"
                     "3,0,2,-1"
                     "-1,3,2,2"
                     "-1,0,-1,0"
                     "0,2,1,-2"
                     "3,0,0,0")))

(define test2 (parse (list
                      "1,-1,0,1"
                      "2,0,-1,0"
                      "3,2,-1,0"
                      "0,0,3,1"
                      "0,0,-1,-1"
                      "2,3,-2,0"
                      "-2,2,0,0"
                      "2,-2,0,-1"
                      "1,-1,0,-1"
                      "3,2,0,2")))

(define test3 (parse (list
                      "1,-1,-1,-2"
                      "-2,-2,0,1"
                      "0,2,1,3"
                      "-2,3,-2,1"
                      "0,2,3,-2"
                      "-1,-1,1,-2"
                      "0,-2,-1,0"
                      "-2,2,3,-1"
                      "1,2,2,0"
                      "-1,-2,0,-2")))

(define (distance p1 p2)
  (apply +
         (map (compose abs -) p1 p2)))

(define (constellation? con pt)
  (ormap (lambda (conpt)
           (<= (distance conpt pt) 3))
         con))

(define (add-constellation constellations star)
  (define (addable? con)
    (constellation? con star))
  (define yes (filter addable? constellations))
  (define no (remove* yes constellations))
  (cons (cons star (append* yes)) no))

(define (build-constellations stars)
  (for/fold ([constellations '()])
            ([star (in-list stars)])
    ;; (displayln constellations)
    (add-constellation constellations star)))

(define input (parse (file->lines "day25_input.txt")))
