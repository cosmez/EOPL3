#lang racket
;; Simple Arithmetic Language.
;; Parser, Interpreter and Compiler
;;

(struct add (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct div (a b) #:transparent)

;; char? -> boolean?
(define (is-valid-number? chr)
  (member chr '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\.)))

;; input-port? -> number?
(define (scan-integer input)
  (string->number
   (apply string
          (for/list ([chr (in-input-port-chars input)]) 
            #:break ((negate is-valid-number?) chr)
            chr))))

;; input-port -> (listof symbol?)
(define (scan input)
  (reverse
   (for/fold ([tokens '()])
             ([chr (in-input-port-chars input)]) 
     #:break (char=? #\) chr)
     (match chr
       [#\( (cons (scan input) tokens)]
       [#\+ (cons 'add tokens)]
       [#\- (cons 'sub tokens)]
       [#\* (cons 'mul tokens)]
       [#\/ (cons 'div tokens)]
       [(? is-valid-number? n) 
        (file-position input (- (file-position input) 1))
        (cons (scan-integer input) tokens)]
       [else tokens]))))

;; (listof symbol?) -> arithmetic-exp?
(define (parse input)
  (match input
    [(list a 'add b)
     (add (parse a) (parse b))]
    [(list a 'sub b)
     (sub (parse a) (parse b))]
    [(list a 'mul b)
     (mul (parse a) (parse b))]
    [(list a 'div b)
     (div (parse a) (parse b))]
    [else input]))


;; arithmetic-exp? -> number?
(define (interp input)
  (match input 
    [(add a b) 
     (+ (interp a) (interp b))]
    [(sub a b) 
     (- (interp a) (interp b))]
    [(mul a b) 
     (* (interp a) (interp b))]
    [(div a b) 
     (let ([result-b (interp b)])
       (if (= result-b 0)
           (error "Cant divide by 0")
           (/ (interp a) result-b)))]
    [else input]))

(interp (parse (scan (open-input-string "20 * (123.54 + (244.3 + 5)) "))))


(module+ test
  (require rackunit)
  (define input (scan (open-input-string "20 * (123.54 + (244.3 + 5)) ")))
  (check-equal? '(20 mul (123.54 add (244.3 add 5))) 
                input)
  (check-equal? (mul 20 (add 123.54 (add 244.3 5))) (parse input))
  (check-equal? (interp (parse input))
               7456.800000000001))
