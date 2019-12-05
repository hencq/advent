#lang racket

(require rackunit
         "../intcode.rkt")

(define input (file->string "input5.txt"))

(define (in) 1)
(define (out x) (printf "> ~a~%" x))

(run input in out)

(define (provide-id) 5)
(run input provide-id out)
