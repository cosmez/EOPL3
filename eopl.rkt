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
;;2.1
;empty-env : () -> Env
(define (empty-env) '(empty-env))

;extend-env : Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (list 'extend-env var val env))

;apply-env : Env x Var -> SchemeVal
(define (apply-env env search-var)
  (cond
    [(eqv? (car env) 'empty-env) (report-no-binding-found search-var)]
    [(eqv? (car env) 'extend-env)
     (let ([saved-var (cadr env)]
           [saved-val (caddr env)]
           [saved-env (caddr env)])
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))]
    [else (report-invalid-env env)]))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad Environment: ~s" env))
    
      

;2.5
;empty-env : () -> Env
(define (empty-env-alist)
  '())

;extende-nv : (Symbol SchemeVal Env) -> Env
(define (extend-env-alist var val env)
  (cons `(,var ,val) env))



;;BinTree := Int | (Symbol Bintree Bintree)
;;ex: 1
;; (foo 1 2)
;; (bar 1 (foo 2))
;; (baz (bar 1 (foo 1 2)) (biz 4 5))

(define-datatype bin-exp bin-exp?
  (node
   (num number?))
  (leaf
   (key symbol?)
   (left bin-exp?)
   (right bin-exp?)))

;;bintree-to-list : (BinTree) -> ListOf(Symbol)
(define (bintree-to-list bintree)
  (cases bin-exp bintree
    (node (num)
         num)
    (leaf (key left right)
          `(interior-node ,key 
                          (leaf-node ,(bintree-to-list left)) 
                          (leaf-node ,(bintree-to-list right))))))


;;max-interior : (BinTree) -> Symbol
(define (max-interior bintree)
  (define (bintree-sums bintree)
    (cases bin-exp bintree
      (node (num)
            `(none ,num 0))
      (leaf (key left right)
            (let* ([bintree-left (bintree-sums left)]
                  [bintree-right (bintree-sums right)]
                  [num-left (cadr bintree-left)]
                  [num-right (cadr bintree-right)]
                  [current-sum (+ num-left num-right)]
                  [max (if (and (> current-sum (caddr bintree-left)) 
                                (> current-sum (caddr bintree-right)))
                           (list key current-sum)
                           (if (> (caddr bintree-left) (caddr bintree-right))
                               (list (car bintree-left) (caddr bintree-left)) 
                               (list (car bintree-right) (caddr bintree-right))))])
              `(,(car max) ,current-sum ,(cadr max))))))
  (car (bintree-sums bintree)))

;; page 18
;; LcExp :: = Identifier
;;        :: = (lambda (Identifier) Lc-exp)
;;        :: = (Lc-exp Lc-exp)

(define-datatype lc-exp lc-exp?
  [var-exp
   (var symbol?)]
  [lambda-exp
   (bound-var symbol?)
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rand lc-exp?)])

(define (report-invalid-concrete-syntax datum)
  (display "invalid syntax"))

;;parse-expression : (SchemeVal) -> LcExp
(define (parse-expression datum)
  (cond
    ((symbol? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp
          (car (cadr datum))
          (parse-expression (caddr datum)))
         (app-exp
          (parse-expression (car datum))
          (parse-expression (cadr datum)))))
    (else (report-invalid-concrete-syntax datum))))


;;unparse-lc-exp : (LcExp) -> SchemeVal
(define (unparse-lc-exp exp)
  (cases lc-exp exp
    (var-exp (var) var)
    (lambda-exp (bound-var body)
                (list 'lambda (list bound-var)
                      (unparse-lc-exp body)))
    (app-exp (rator rand)
             (list
              (unparse-lc-exp rator) (unparse-lc-exp rand)))))