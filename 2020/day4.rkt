#lang racket

(define example "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(define (split-by xs pred)
  (let loop ([result '()]
             [current '()]
             [xs xs])
    (if (empty? xs)
        (reverse (cons (reverse current) result))
        (let ([x (car xs)])
          (if (pred x)
              (loop (cons (reverse current) result) '() (cdr xs))
              (loop result (cons x current) (cdr xs)))))))

(define (split-entries lines)
  (map (lambda (entry)
         (append-map string-split entry))
       (split-by lines (lambda (s)  (equal? s "")))))

(define (entry->map entry)
  (for/hash ([fld (in-list entry)])
    (match (string-split fld ":")
      [(list key val) (values key val)])))

(define (valid? passport)
  (for/and ([fld (in-list (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))])
    (hash-has-key? passport fld)))

(define (lines->passports lines)
  (map entry->map (split-entries lines)))
(append-map string-split (car (split-by (string-split example "\n") (lambda (s)  (equal? s "")))))

(map valid? (lines->passports (string-split example "\n")))
(define input (file->lines "input4.txt"))
(length (filter valid? (lines->passports input)))


;;;; Part 2

(define (year-between min max)
  (lambda (val)
    (and
     (regexp-match (pregexp "^\\d{4}$") val)
     (let ([num (string->number val)])
       (and (>= num min) (<= num max))))))

(define byr (year-between 1920 2002))
(define iyr (year-between 2010 2020))
(define eyr (year-between 2020 2030))

(define (hgt val)
  (define m (regexp-match (pregexp "^(\\d+)(cm|in)$") val))
  (and m
       (let ([num (string->number (cadr m))]
             [unit (caddr m)])
         (if (equal? unit "in")
             (and (>= num 59) (<= num 76))
             (and (>= num 150) (<= num 193))))))

(define (hcl val)
  (regexp-match-exact? (pregexp "#[0-9a-f]{6}") val))

(define (ecl val)
  (regexp-match-exact? "(amb|blu|brn|gry|grn|hzl|oth)" val))

(define (pid val)
  (regexp-match-exact? (pregexp "\\d{9}") val))

(define (valid-strict? passport)
  (define rules (hash "byr" byr
                      "iyr" iyr
                      "eyr" eyr
                      "hgt" hgt
                      "hcl" hcl
                      "ecl" ecl
                      "pid" pid))
  (for/and ([(key validator) (in-hash rules)])
    (and (hash-has-key? passport key)
         (validator (hash-ref passport key)))))

(define invalid "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(define valid "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")
