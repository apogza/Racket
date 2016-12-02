#lang racket
;;permutations

;; A special kind of a map. Apply a procedure using element x onto list y
;; it is going to be used in the permute procedure. After having removed
;; elem and computed its permutations simply cons the elem too its permutations
;; using mapxy and cons as the proc.
(define (mapxy proc x y)
  (cond
    ((empty? y) empty)
    (else
     (cons (proc x (car y)) (mapxy proc x (cdr y))))))

;;compute the permutations of an element with regards to a list
(define (permute elem l)
  (cond
    ((= (length l) 1) l)
    ((= (length l) 2) (list l (reverse l)))
    (else
     (mapxy cons elem (permutations (remove elem l))))))

;; compute the permutations of the subsets by removing element by element while traversing
;; keep the original list and perform the removal action onto it.
;; use myl to traverse and shorten the list by a car each time
(define (traverse-permute myl orig)
  (cond
    ((= (length orig) 2) (list orig (reverse orig)))
    ((empty? myl) empty)
    (else
     (append (permute (car myl) orig) (traverse-permute (cdr myl) orig)))))

;;flatten a list of lists
(define (flattenx llist)
  (cond
    ((empty? llist) empty)
    (else
     (append (car llist) (flattenx (cdr llist))))))

(define (combination curl restl n)
  (cond
    ((= (length curl) n) (list curl))
    ((= (+ (length curl) (length restl)) n) (list (append curl restl)))
    (else
     (append (combination (append curl (list (car restl))) (cdr restl) n) (combination curl (cdr restl) n)))))

;;the main function that calculates all permutation of a list
(define (permutations myl)
  (traverse-permute myl myl))

;;calculate permutations of n symbols of a list
(define (n-permutations myl n)
  (flattenx (map permutations (n-combinations myl n))))

;;compute all possible combinations of a list of n symbols
(define (n-combinations myl n)
  (cond
    ((< (length myl) n) empty)
    ((= (length myl) n) (list myl))
    (else
     (append (combination (list (car myl)) (cdr myl) n) (n-combinations (cdr myl) n)))))
