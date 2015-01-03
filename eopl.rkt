#lang eopl
;; chapter 01 code
;; page 13
(define (list-length lst)
  (if (null? lst) 0
      (+ 1 (list-length (cdr lst)))))

;; page 14
(define (nth-element lst index)
  (if (= index 0) (car lst)
      (nth-element (cdr lst) (-  index 1))))

;; page 16
(define (remove-first lst symbol)
  (cond
    [(null? lst) lst]
    [(equal? (car lst) symbol) (cdr lst)]
    [else (cons (car lst) (remove-first (cdr lst) symbol))]))


;; page 18
;; lexpr =
;;   var
;;   (lambda (var) (lexpr))
;;   (lexpr lexpr)
(define (occurs-free? var expr)
  (cond 
    [(symbol? expr) (eqv? var expr)] ;variable case
    [(eqv? (car expr) 'lambda) ;lambda case
     (and (not (eqv? (caadr expr) var))
          (occurs-free? var (caddr expr)))]
    [else (or (occurs-free? var (car expr))
              (occurs-free? var (cadr expr)))]))


;; page 20
(define (subst-in-sexp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (subst new old sexp)))

;; page 20
(define (subst new old sexp)
  (cond 
    [(null? sexp) sexp]
    [else (cons (subst-in-sexp new old (car sexp)) (subst new old (cdr sexp)))]))

;;page 23
(define (number-elements lst)
  (define (number-elements-from lst i)
    (if (null? lst) lst 
        (cons (list i (car lst))
              (number-elements-from (cdr lst) (+ 1 i)))))
  (number-elements-from lst 0))

;;page 24
(define (list-sum lst)
  (if (null? lst) 0 
      (+ (car lst) (list-sum (cdr lst)))))

;;page 26
;duple Int x SchemeVal -> ListOf(SchemeVal)
(define (duple n x)
  (if (zero? n) '()
      (cons x (duple (- n 1) x))))

;;page 26
;;invert ListOf(Pair) -> ListOf(Pair)
(define (invert lst)
  (define (invert-pair pair)
    (list (cadr pair) (car pair)))
  (if (null? lst) lst
      (cons (invert-pair (car lst)) (invert (cdr lst)))))

;;page 26
;;down ListOf(SchemeVal) -> ListOf(ListOf(SchemeVal))
(define (down lst)
  (if (null? lst) lst
      (cons (list (car lst)) (down (cdr lst)))))

;;page 27
;;swapper ListOf(SchemeVal) -> ListOf(SchemeVal)
(define (swapper s1 s2 slist)
  (define (swap lst)
    (if (symbol? lst) 
        (cond 
          [(eqv? lst s1) s2]
          [(eqv? lst s2) s1]
          [else lst])
        (swapper s1 s2 lst)))
  (if (null? slist) slist
      (cons (swap (car slist)) (swapper s1 s2 (cdr slist)))))

;;page 27
;;list-set ListOf(SchemeVal) x Number x SchemeVal -> ListOf(SchemeVal)
(define (list-set lst n x)
  (if (null? lst) lst
      (cons (if (zero? n) x (car lst)) 
            (list-set (cdr lst) (- n 1) x))))

;;page 27
;;count-occurrences Symbol ListOf(SchemeVal) -> Number
(define (count-occurrences s lst)
  (define (count-occurrence value)
    (if (symbol? value)
        (if (equal? s value) 1 0)
        (count-occurrences s value)))
  (if (null? lst) 0
      (+ (count-occurrence (car lst)) (count-occurrences s (cdr lst)))))

;;page 27
;;product ListOf(SchemeVal) X ListOf(SchemeVal) -> ListOf(Pair(SchemeVal))
(define (product sos1 sos2)
  (define (product-1 lst1 lst2)
  (cond 
    [(null? lst2) '()]
    [(null? lst1) (product-1 sos1 (cdr lst2))]
    [else (cons (list (car lst1) (car lst2)) (product-1 (cdr lst1) lst2))]))
  (product-1 sos1 sos2))

;;page 27
;;filter-in Function X ListOf(SchemeVal) -> ListOf(SchemeVal)
(define (filter-in pred? lst)
  (if (null? lst) lst
      (if (pred? (car lst))
          (cons (car lst) (filter-in pred? (cdr lst)))
          (filter-in pred? (cdr lst)))))

;;page 27
;;list-index Function X ListOf(SchemeVal) -> Number
(define (list-index pred? lst)
  (define (list-index-counter pred? lst i)
    (if (null? lst) #f
     (if (pred? (car lst)) i 
         (list-index-counter pred? (cdr lst) (+ i 1)))))
  (list-index-counter pred? lst 0))

;;page 28
;;every? Function X ListOf(Symbol) -> Boolean
(define (every? pred? lst)
  (if (null? lst) #t
      (and (pred? (car lst)) (every? pred? (cdr lst)))))

;;page 28
;;every? Function X ListOf(Symbol) -> Boolean
(define (exists? pred? lst)
  (if (null? lst) #f
      (or (pred? (car lst)) (exists? pred? (cdr lst)))))

;;page 28
;;up ListOf(List) -> ListOf(List)
(define (up lst)
  (define (up-1 lst-car lst-rest)
    (if (null? lst-car) 
        (up lst-rest)
        (cons (car lst-car) (up-1 (cdr lst-car) lst-rest))))
  (cond 
    [(null? lst) lst]
    [(list? (car lst)) (up-1 (car lst) (cdr lst))]
    [else (cons (car lst) (up (cdr lst))) ]))

;;page 28
;;flatten list? -> list?
(define (flatten lst)
  (if (every? (lambda (el) (not (list? el))) lst) lst
      (flatten (up lst))))

;;page 28
;;merge ListOf(Integer) x ListOf(Integer) -> ListOf(Integer)
(define (merge loi1 loi2)
  (if (null? loi1) loi2
      (cons (car loi1) (merge (cdr loi1) loi2))))

;;remove-at ListOf(SchemeVal) x Integer -> ListOf(SchemeVal)
(define (remove-at lst index)
  (define (remove-1 lst i)
    (cond
      [(null? lst) lst]
      [(= index i) (cdr lst)]
      [else (cons (car lst) (remove-1 (cdr lst) (+ i 1)))]))
  (remove-1 lst 0))


;;page 28
;;sort ListOf(Integer) -> ListOf(Integer)
(define (sort integer-list)
  (define (partition pivot lst tmp1 tmp2)
    (cond 
      [(null? lst) (list tmp1 tmp2)]
      [(< (car lst) pivot) (partition pivot (cdr lst) (cons (car lst) tmp1) tmp2)]
      [else (partition pivot (cdr lst) tmp1 (cons (car lst) tmp2))]))
  (if (or (null? integer-list) 
          (eqv? 1 (list-length integer-list))) 
      integer-list
      (let* ([pivot (list-ref integer-list (floor (/ (list-length integer-list) 2)))]
            [pivotless-list (remove-at integer-list (floor (/ (list-length integer-list) 2)))]
            [result (partition pivot pivotless-list '() '())])
        (merge (merge (sort (car result)) (list pivot)) (sort (cadr result))))))

;;page 29
;;sorted-merge ListOf(Integer) x ListOf(Integer) -> ListOf(Integer)
(define (sorted-merge loi1 loi2)
  (sort (merge loi1 loi2)))

;;page 29
;;sort ListOf(Integer) x Function -> ListOf(Integer)
(define (sort/predicate integer-list pred?)
  (define (partition pivot lst tmp1 tmp2)
    (cond 
      [(null? lst) (list tmp1 tmp2)]
      [(pred? (car lst) pivot) (partition pivot (cdr lst) (cons (car lst) tmp1) tmp2)]
      [else (partition pivot (cdr lst) tmp1 (cons (car lst) tmp2))]))
  (if (or (null? integer-list) 
          (eqv? 1 (list-length integer-list))) 
      integer-list
      (let* ([pivot (list-ref integer-list (floor (/ (list-length integer-list) 2)))]
            [pivotless-list (remove-at integer-list (floor (/ (list-length integer-list) 2)))]
            [result (partition pivot pivotless-list '() '())])
        (merge (merge (sort/predicate (car result) pred?) (list pivot)) 
               (sort/predicate (cadr result) pred?)))))


;;Chapter 02

;2.5
;empty-env : () -> Env
(define (empty-env-alist)
  '())

;extende-nv : (Symbol SchemeVal Env) -> Env
(define (extend-env-alist var val env)
  (cons `(,var ,val) env))


;2.7
(define (empty-env-1)
  #())

;;BinTree := Int | (Symbol Bintree Bintree)
;;ex: 1
;; (foo 1 2)
;; (bar 1 (foo 2))
;; (baz (bar 1 (foo 1 2)) (biz 4 5))