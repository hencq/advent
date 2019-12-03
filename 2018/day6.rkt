#lang racket
(require rackunit)

(struct rect (left top right bottom) #:transparent)

(define (bounding-rect coords)
  (define xs (map car coords))
  (define ys (map cdr coords))
  (rect (apply min xs) (apply min ys) (apply max xs) (apply max ys)))


(define (distance p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

(define (closest-coord p coords)
  (define-values (coord _)
    (for/fold ([closest 0]
               [mindist +inf.0])
              ([c (in-list coords)]
               [i (in-range (length coords))])
      (define dist (distance p c))
      (cond
        [(< dist mindist) (values i dist)]
        [(= dist mindist) (values #f dist)]
        [else (values closest mindist)])))
  coord)

(define (draw-area coords)
  (define bound (bounding-rect coords))
  (for ([y (in-range (rect-top bound) (add1 (rect-bottom bound)))])
    (for ([x (in-range (rect-left bound) (add1 (rect-right bound)))])
      (define closest (closest-coord (cons x y) coords))
      (if closest
          (display closest)
          (display ".")))
    (displayln "")))

(define (areas coords)
  (define finite (finite-area-centers coords))
  (define bound (bounding-rect coords))
  (for*/fold ([areas (hash)])
             ([x (in-range (rect-left bound) (add1 (rect-right bound)))]
              [y (in-range (rect-top bound) (add1 (rect-bottom bound)))])
    (hash-update areas (closest-coord (cons x y) coords)
                 add1 0)))

(define (finite-areas coords)
  (define finite (finite-area-centers coords))
  (define bound (bounding-rect coords))
  (define areas 
    (for*/fold ([areas (hash)])
               ([x (in-range (rect-left bound) (add1 (rect-right bound)))]
                [y (in-range (rect-top bound) (add1 (rect-bottom bound)))])
      (hash-update areas (closest-coord (cons x y) coords)
                   add1 0)))
  (map (curry hash-ref areas) finite))

(define (finite-area-centers coords)
  (define bound (bounding-rect coords))
  (define edge (append
                (map (curry cons (rect-left bound)) (range (rect-top bound)
                                                           (rect-bottom bound)))
                (map (curry cons (rect-right bound)) (range (rect-top bound)
                                                           (rect-bottom bound)))
                (map (curry cons (rect-top bound)) (range (rect-left bound)
                                                          (rect-right bound)))
                (map (curry cons (rect-bottom bound)) (range (rect-left bound)
                                                             (rect-right bound)))))
  (define infinite-coords (list->set (for/list ([c edge])
                                       (closest-coord c coords))))
  (for/list ([i (in-range (length coords))]
             #:unless (set-member? infinite-coords i))
    i))

(define (total-distance p coords)
  (apply + (map (curry distance p) coords)))

(define (total-distance-within n coords)
  (define bound (bounding-rect coords))
  (for*/list ([x (in-range (rect-left bound) (add1 (rect-right bound)))]
              [y (in-range (rect-top bound) (add1 (rect-bottom bound)))]
              #:when (< (total-distance (cons x y) coords) n))
    (cons x y)))

(module+ test
  (define coords (list 
                  (cons 1 1)
                  (cons 1 6)
                  (cons 8 3)
                  (cons 3 4)
                  (cons 5 5)
                  (cons 8 9)))
  
  (check-eq? (apply max (finite-areas coords)) 17)
  (check-eq? (total-distance (cons 4 3) coords) 30)
  (length (total-distance-within 32 coords))
  (draw-area coords))

(define coords (list
                (cons 158 163)
                (cons 287 68)
                (cons 76 102)
                (cons 84 244)
                (cons 162 55)
                (cons 272 335)
                (cons 345 358)
                (cons 210 211)
                (cons 343 206)
                (cons 219 323)
                (cons 260 238)
                (cons 83 94)
                (cons 137 340)
                (cons 244 172)
                (cons 335 307)
                (cons 52 135)
                (cons 312 109)
                (cons 276 93)
                (cons 288 274)
                (cons 173 211)
                (cons 125 236)
                (cons 200 217)
                (cons 339 56)
                (cons 286 134)
                (cons 310 192)
                (cons 169 192)
                (cons 313 106)
                (cons 331 186)
                (cons 40 236)
                (cons 194 122)
                (cons 244 76)
                (cons 159 282)
                (cons 161 176)
                (cons 262 279)
                (cons 184 93)
                (cons 337 284)
                (cons 346 342)
                (cons 283 90)
                (cons 279 162)
                (cons 112 244)
                (cons 49 254)
                (cons 63 176)
                (cons 268 145)
                (cons 334 336)
                (cons 278 176)
                (cons 353 135)
                (cons 282 312)
                (cons 96 85)
                (cons 90 105)
                (cons 354 312)))
