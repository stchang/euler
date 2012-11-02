#lang racket

;; max-tri : [ListOf [ListOf Number]] -> Number
;; The triangle must have at least one number 
;; (ie, the input is at least a non-empty list of a non-empty list).
;; Each subsequent list (ie "row") must have one more element than the previous.
(define (max-tri tri)
  (if (null? (rest tri))
      (apply max (first tri))
      (max-tri (cons (combine-two-rows (first tri) (second tri)) (rest (rest tri))))))

;; combine-two-rows : [ListOf Number] [ListOf Number] -> [ListOf Number]
;; row2 always has one more element than row1
(define/match (combine-two-rows row1 row2)
  [((list x) (list y z)) (list (+ x y) (+ x z))]
  [((list-rest x rest1) (list-rest y z rest2)) 
   (define res (combine-two-rows rest1 (cons z rest2)))
   (cons (+ x y) (cons (max (+ x z) (first res)) (rest res)))])


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
  
  (check-equal? (max-tri tri1) 23)
  (check-equal? (max-tri tri2) 1074)
  (check-equal? (max-tri tri3) 7273))


