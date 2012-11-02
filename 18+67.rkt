#lang racket

;; ----------------------------------------------------------------------------
;; top-down

;; max-tri-route : [ListOf [ListOf Number]] -> Number
;; The triangle must have at least one number 
;; (ie, the input is at least a non-empty list of a non-empty list).
;; Each subsequent list (ie "row") must have one more element than the previous.
(define (max-tri-route tri)
  (if (null? (rest tri))
      (apply max (first tri))
      (max-tri-route 
       (cons (combine-two-rows (first tri) (second tri)) 
             (rest (rest tri))))))

;; combine-two-rows : [ListOf Number] [ListOf Number] -> [ListOf Number]
;; - row2 always has one more element than row1
;; - produces list of length row2
(define/match (combine-two-rows row1 row2)
  [((list x) (list y z)) 
   (list (+ x y) (+ x z))]
  [((list-rest x rest1) (list-rest y z rest2)) 
   (define res (combine-two-rows rest1 (cons z rest2)))
   (cons (+ x y) (cons (max (+ x z) (first res)) (rest res)))])

;; ----------------------------------------------------------------------------
;; bottom-up

(define (max-tri-route2 tri) (max-tri/bottom-up (reverse tri)))

(define (max-tri/bottom-up tri)
  (if (null? (rest tri))
      (first (first tri))
      (max-tri/bottom-up 
       (cons (combine-two-rows/bottom-up (second tri) (first tri))
             (rest (rest tri))))))

;; row2 always has one more element than row1
(define/match (combine-two-rows/bottom-up row1 row2)
  [((list x) (list y z))
   (list (+ x (max y z)))]
  [((list-rest x rest1) (list-rest y z rest2))
   (cons (+ x (max y z)) (combine-two-rows/bottom-up rest1 (cons z rest2)))])

;; ----------------------------------------------------------------------------
;; bottom-up, with foldl
(define (max-tri-route3 tri)
  (define rev-tri (reverse tri))
  (first (foldl combine-two-rows/bottom-up (first rev-tri) (rest rev-tri))))

;; ----------------------------------------------------------------------------
;; bottom-up, with foldr1

(define/match (foldr1 f lst)
  [(_ (list x))           x                    ]
  [(_ (list-rest x rest)) (f x (foldr1 f rest))])

(define (max-tri-route4 tri) (first (foldr1 combine-two-rows/bottom-up tri)))


  
(module+ test
  (require rackunit)
  
  ;; problem 18 examples
  (define tri1 
    '((3)
      (7 4)
      (2 4 6)
      (8 5 9 3)))

  (define tri2
    '((75)
      (95 64)
      (17 47 82)
      (18 35 87 10)
      (20 04 82 47 65)
      (19 01 23 75 03 34)
      (88 02 77 73 07 63 67)
      (99 65 04 28 06 16 70 92)
      (41 41 26 56 83 40 80 70 33)
      (41 48 72 33 47 32 37 16 94 29)
      (53 71 44 65 25 43 91 52 97 51 14)
      (70 11 33 28 77 73 17 78 39 68 17 57)
      (91 71 52 38 17 14 91 43 58 50 27 29 48)
      (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
      (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

  ;; problem 67 example
  (define tri3
    (for/list ([line (in-lines (open-input-file "triangle.txt"))])
      (map string->number (string-split line))))

  (check-true 
   (= (max-tri-route  tri1) (max-tri-route2 tri1) 
      (max-tri-route3 tri1) (max-tri-route4 tri1) 
      23))
  (check-true 
   (= (max-tri-route  tri2) (max-tri-route2 tri2) 
      (max-tri-route3 tri2) (max-tri-route4 tri2) 
      1074))
  (check-true
   (= (max-tri-route  tri3) (max-tri-route2 tri3) 
      (max-tri-route3 tri3) (max-tri-route4 tri3) 
      7273)))

