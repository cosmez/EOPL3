#lang racket

(provide/contract
 [list-index (-> (listof symbol?) symbol? exact-nonnegative-integer?)])

;; helper functions for the static environment
(define (list-index lst el)
  (if (eq? (car lst) el) 
      0
      (add1 (list-index (cdr lst) el))))

