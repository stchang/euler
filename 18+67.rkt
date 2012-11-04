#lang racket

;; A Row is a list of at least one number.

;; A Triangle is a list of at least one Row, 
;;  where each row has one more number than the previous row.

;; ----------------------------------------------------------------------------
;; top-down solution

;; max-tri-route : Triangle -> Number
;; Computes the maximum total when moving from top of triangle to bottom.
(define/match (max-tri-route tri)
  [((list a-single-row)) 
   (apply max a-single-row)]
  [((list-rest row1 row2 rest-rows))
   (max-tri-route (cons (process-row row1 row2) rest-rows))])

;; process-row : Row Row -> Row
;; Takes a list of intermediate maximum values and a row, and incorporates
;;  the given row into the intermediate values.
;; - new-row always has one more element than tmp-maxes
;; - produces list of length new-row
(define/match (process-row tmp-maxes new-row)
  [((list x) (list y z)) 
   (list (+ x y) (+ x z))]
  [((list-rest x rest-maxes) (list-rest y z rest-row)) 
   (define res (process-row rest-maxes (cons z rest-row)))
   (cons (+ x y) (cons (max (+ x z) (first res)) (rest res)))])


;; ----------------------------------------------------------------------------
;; bottom-up solution

(define (max-tri-route2 tri) (max-tri/bottom-up (reverse tri)))

;; Computes total starting from bottom row.
(define/match (max-tri/bottom-up tri)
  [((list (list the-max-total))) 
   the-max-total]
  [((list-rest row1 row2 rest-rows))
   (max-tri/bottom-up (cons (process-row/bottom-up row2 row1) rest-rows))])

;; - tmp-maxes always has one more element than new-row
;; - produces list of length new-row
(define/match (process-row/bottom-up new-row tmp-maxes)
  [((list x) (list y z))
   (list (+ x (max y z)))]
  [((list-rest x rest-row) (list-rest y z rest-maxes))
   (cons (+ x (max y z)) (process-row/bottom-up rest-row (cons z rest-maxes)))])


;; ----------------------------------------------------------------------------
;; bottom-up solution, with foldl

(define (max-tri-route3 tri)
  (define rev-tri (reverse tri))
  (first (foldl process-row/bottom-up (first rev-tri) (rest rev-tri))))


;; ----------------------------------------------------------------------------
;; bottom-up solution, with foldr1

(define/match (foldr1 f lst)
  [(_ (list x)) x]
  [(_ (list-rest x rest)) (f x (foldr1 f rest))])

(define (max-tri-route4 tri) (first (foldr1 process-row/bottom-up tri)))


  
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

