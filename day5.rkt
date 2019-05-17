#lang racket
(require rackunit
         threading)

(define (react? a b)
  (and (not (eq? a b))
       (eq? (char-downcase a) (char-downcase b))))

(define (remove-reacting str)
  (let loop ([result '()]
             [chars (string->list str)])
    (cond
      [(empty? chars)  (length result)]
      [(empty? result) (loop (cons (car chars) result) (cdr chars))]
      [(react? (car result) (car chars)) (loop (cdr result) (cdr chars))]
      [else (loop (cons (car chars) result) (cdr chars))])))

(define (available-chars str)
  (list->set (string->list (string-downcase str))))

(define (remove-char-pair str char)
  (~> str
      (string-replace (string (char-downcase char)) "")
      (string-replace (string (char-upcase char)) "")))

(define (find-shortest str)
  (apply min
         (for/list ([ch (in-set (available-chars str))])
           (remove-reacting (remove-char-pair str ch)))))


(module+ test
  (define test-seq "dabAcCaCBAcCcaDA")
  (check-eq? (remove-reacting test-seq) 10)
  (check-eq? (find-shortest test-seq) 4))

(define input (string-trim (file->string "day5_input.txt")))



