#lang racket

(require "../utils.rkt")

(struct node (type multi batch) #:transparent)

(define (match-line line)
  (define clauses (string-split line " => "))
  (define parts (cons (cadr clauses) (string-split (car clauses) ", ")))
  (for/list ([p (in-list parts)])
    (match-define (list amt type) (string-split p " "))
    (cons (string->number amt) type)))

;; First build a tree that maps each chemical to its dependents
;; For each edge store the multiplier and the batch size
(define (input->tree input)
  (define inputs (map match-line (string-split input "\n")))
  (for/fold ([tree (hash)])
            ([line (in-list inputs)])
    (match-define (cons batch dep) (car line))
    (for/fold ([tree tree])
              ([parent (in-list (cdr line))])
      (match-define (cons multi name) parent)
      (hash-update tree name (lambda (nodes)
                               (cons (node dep multi batch) nodes)) '()))))

;; To calculate the amount of ore needed, ask all dependents how much they need
;; For each dependent calculate the number of batches needed and multiply with
;; the multiplier
(define (calc-ore tree [req 1])
  (let walk ([node "ORE"])
    (if (equal? node "FUEL")
        req
        (let ([deps (hash-ref tree node '())])
          (foldl (lambda (n sum)
                   (+ sum (* (node-multi n)
                             (ceiling (/ (walk (node-type n)) (node-batch n))))))
                 0
                 deps)))))

(module+ test
  (require rackunit)
  (define test "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

  (define test2 "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

    (define test3 "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")
    
  (define test4 "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")

  (check-equal? (calc-ore (input->tree test)) 31)
  (check-equal? (calc-ore (input->tree test2)) 165)
  (check-equal? (calc-ore (input->tree test3)) 13312)
  (check-equal? (calc-ore (input->tree test4)) 2210736))


(define input (file->string "input14.txt"))
(define tree (input->tree input))
(calc-ore tree)


;; Just brute force it by binary searching for the answer
(define (part2 tree)
  (define cap 1000000000000)
  (define lower (quotient cap (calc-ore tree)))
  (let loop ([low lower]
             [up (* 2 lower)])
    (define req (calc-ore tree up))
    (if (> req cap)
        ;; If there are no numbers between the upper and lower bound we stop
        (let ([delta (quotient (- up low) 2)])
          (if (= delta 0)
              low
              (loop low (+ low delta))))
        (loop up (* 2 up)))))

(module+ test
  (check-equal? (part2 (input->tree test3)) 82892753)
  (check-equal? (part2 (input->tree test4)) 460664))

(part2 tree)
