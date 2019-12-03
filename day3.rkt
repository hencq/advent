#lang racket

(require rackunit
         threading)

(define (parse-line line)
  (define runs (string-split line ","))
  (apply append
         (for/list ([run (in-list runs)])
           (define dir (string-ref run 0))
           (define steps (string->number (substring run 1)))
           (make-list steps dir))))

(define (fill-map map dirs)
  (let loop ([map map]
             [xpos 0]
             [ypos 0]
             [dirs dirs])
    (if (empty? dirs)
        map
        (loop (hash-set map (cons xpos ypos) #t)
              (+ xpos (case (car dirs)
                        [(#\L) -1]
                        [(#\R) 1]
                        [else 0]))
              (+ ypos (case (car dirs)
                        [(#\D) -1]
                        [(#\U) 1]
                        [else 0]))
              (cdr dirs)))))

(define (closest-intersect l1 l2)
  (define m1 (fill-map (hash) (parse-line l1)))
  (define m2 (fill-map (hash) (parse-line l2)))
  (define intercepts (for/list ([(pos v) (in-hash m2)]
                                #:when (and v (hash-ref m1 pos #f)))
                       pos))
  (cadr
   (sort (map (lambda (x)
                (+ (abs (car x)) (abs (cdr x))))
              intercepts)
         <)))

(define lines (file->lines "input3.txt"))

(module+ test
  (let ([ l1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"]
        [l2 "U62,R66,U55,R34,D71,R55,D58,R83"])
    (check-eq? (closest-intersect l1 l2) 159))

  (let ([l1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"]
        [l2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
    (check-eq? (closest-intersect l1 l2) 135)))

(closest-intersect (car lines) 
                   (cadr lines))


(define (fill-map2 map dirs)
  (let loop ([map map]
             [xpos 0]
             [ypos 0]
             [steps 0]
             [dirs dirs])
    (if (empty? dirs)
        map
        (loop (hash-set map (cons xpos ypos) (hash-ref map (cons xpos ypos) steps))
              (+ xpos (case (car dirs)
                        [(#\L) -1]
                        [(#\R) 1]
                        [else 0]))
              (+ ypos (case (car dirs)
                        [(#\D) -1]
                        [(#\U) 1]
                        [else 0]))
              (add1 steps)
              (cdr dirs)))))

(define (intersect-steps l1 l2)
  (define m1 (fill-map2 (hash) (parse-line l1)))
  (define m2 (fill-map2 (hash) (parse-line l2)))
  (define steps (for/list ([(pos v) (in-hash m2)]
                           #:when (hash-has-key? m1 pos))
                  (+ v (hash-ref m1 pos))))
  (cadr
   (sort steps <)))

(module+ test
  (let ([l1 "R8,U5,L5,D3"]
        [l2 "U7,R6,D4,L4"])
    (check-eq? (intersect-steps l1 l2) 30))

  (let ([l1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"]
        [l2 "U62,R66,U55,R34,D71,R55,D58,R83"])
    (check-eq? (intersect-steps l1 l2) 610))

  (let ([l1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"]
        [l2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
    (check-eq? (intersect-steps l1 l2) 410)))

(intersect-steps (car lines) (cadr lines))





