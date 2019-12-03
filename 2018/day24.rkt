#lang racket

(struct group (type units hp weak immune ap at initiative) #:transparent)

(define (parse-line line type)
  (define rx (pregexp "(\\d+) units each with (\\d+) hit points (\\((weak|immune) to ([\\w, ]+)(; (weak|immune) to ([\\w, ]+))?\\))? ?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)"))
  (define parts (regexp-match rx line))
  (if parts
      (let* ([units (string->number (cadr parts))]
            [hp (string->number (caddr parts))]
            [mod1 (if (list-ref parts 3)
                      (cons (list-ref parts 4) (string-split (list-ref parts 5) ", "))
                      '(()))]
            [mods (list (if (list-ref parts 6)
                            (cons (list-ref parts 7) (string-split (list-ref parts 8) ", "))
                            '(()))
                        mod1)]
            [ap (string->number (list-ref parts 9))]
            [at (list-ref parts 10)]
            [initiative (string->number (list-ref parts 11))])
        (define immune (dict-ref mods "immune" '()))
        (define weak (dict-ref mods "weak" '()))
        (group type units hp weak immune ap at initiative))
      #f))

(define (parse lines)
  (let loop ([immune '()]
             [infection '()]
             [todo (cdr lines)]
             [i 0])
    (if (empty? todo)
        (append immune infection)
        (let ([line (car todo)])
          (cond
            [(equal? line "") (loop immune
                                    (list (cons i (parse-line (caddr todo) 'infection)))
                                    (cdddr todo)
                                    (add1 i))]
            [(cons? infection) (loop immune
                                     (cons (cons i (parse-line line 'infection)) infection)
                                     (cdr todo)
                                     (add1 i))]
            [else (loop (cons (cons i (parse-line line 'immune)) immune)
                        infection
                        (cdr todo)
                        (add1 i))])))))

(define (ep group)
  (* (group-ap group) (group-units group)))

(define test (parse
              (list
               "Immune System:"
               "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
               "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
               ""
               "Infection:"
               "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
               "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")))

(define (damage attacker defender)
  (cond
    [(set-member? (group-immune defender) (group-at attacker)) 0]
    [(set-member? (group-weak defender) (group-at attacker)) (* 2 (ep attacker))]
    [else (ep attacker)]))

(define (attack attacker defender)
  (define killed (quotient (damage attacker defender) (group-hp defender)))
  (struct-copy group defender [units (max 0 (- (group-units defender) killed))]))

(define (sort-attackers groups)
  (sort groups (lambda (g1 g2)
                 (or (> (ep g1) (ep g2))
                     (and (= (ep g1) (ep g2))
                          (> (group-initiative g1) (group-initiative g2)))))
        #:key cdr))

(define (sort-targets attacker groups)
  (define targets (filter (lambda (g)
                            (and
                             (not (equal? (group-type attacker)
                                          (group-type (cdr g))))
                             (> (damage attacker (cdr g)) 0)))
                          groups))
  (sort targets (lambda (g1 g2)
                  (or (> (damage attacker g1) (damage attacker g2))
                      (and (= (damage attacker g1) (damage attacker g2))
                           (or (> (ep g1) (ep g2))
                               (and (= (ep g1) (ep g2))
                                    (> (group-initiative g1) (group-initiative g2)))))))
        #:key cdr))


(define (select-targets groups)
  (for/fold ([groups groups]
             [targets '()]
             #:result targets)
            ([(i g) (in-dict (sort-attackers groups))])
    (define sorted (sort-targets g groups))
    (if (cons? sorted)
        (let ([target (car sorted)])
          (values (dict-remove groups (car target)) (cons (cons i (car target))
                                                          targets)))
        (values groups targets))))

(define (fight-1 groups)
  (define targets (sort (select-targets groups) >
                        #:key (lambda (pair)
                                (group-initiative (dict-ref groups (car pair))))))
  (for/fold ([groups groups]
             #:result (filter (lambda (g) (> (group-units (cdr g)) 0)) groups))
            ([(ai di) (in-dict targets)])
    (define attacker (dict-ref groups ai))
    (if (> (group-units attacker) 0)
        (dict-update groups di (lambda (def)
                                 (attack attacker def)))
        groups)))

(define (stalemate? groups)
  (equal? groups (fight-1 groups)))

(define (fight-over? groups)
  (andmap (lambda (g)
            (equal? (group-type g) (group-type (cdr (car groups)))))
          (map cdr (cdr groups))))

(define (fight groups)
  (let loop ([groups groups])
    (if (or (fight-over? groups) (stalemate? groups))
        groups
        (loop (fight-1 groups)))))


(define (part1 groups)
  (define survivors (fight groups))
  (apply + (map group-units (map cdr survivors))))

(define input (parse (file->lines "day24_input.txt")))

(define (boost groups num)
  (map (lambda (pair)
         (define index (car pair))
         (define g (cdr pair))
         (if (equal? (group-type g) 'immune)
             (cons index (struct-copy group g [ap (+ (group-ap g) num)]))
             pair))
       groups))

(define (immune-won? groups)
  (and (equal? (group-type (cdar groups)) 'immune)
       (fight-over? groups)))

(define (part2 groups)
  (let loop ([bst 0])
    (define result (fight (boost groups bst)))
    (if (immune-won? result)
        result
        (loop (add1 bst)))))

;; Slightly faster version that doubles the boost instead of linearly increases
(define (part2-log groups)
  (let loop ([base 0]
             [bst 1])
    (define result (fight (boost groups (+ base bst))))
    (if (immune-won? result)
        (if (= bst 1)
            result
            (loop (+ base (/ bst 2)) 1))
        (loop base (* 2 bst)))))
