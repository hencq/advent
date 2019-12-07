#lang racket

(require rackunit
         "../intcode.rkt")

(define input (new-prog (file->string "input5.txt")))

(run! input '(1))

(restart! input)
(run! input '(5))
