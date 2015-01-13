#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(struct const-exp (num) #:transparent)
(struct diff-exp (exp1 exp2) #:transparent)
(struct zero?-exp (exp1) #:transparent)
(struct equal?-exp (exp1 exp2) #:transparent)
(struct greater?-exp (exp1 exp2) #:transparent)
(struct less?-exp (exp1 exp2) #:transparent)
(struct if-exp (exp1 exp2 exp3) #:transparent)
(struct var-exp (var) #:transparent)
(struct let-exp (var exp1 body) #:transparent)
(struct add-exp (exp1 exp2) #:transparent)
(struct minus-exp (exp) #:transparent)
(struct mul-exp (exp1 exp2) #:transparent)
(struct quotient-exp (exp1 exp2) #:transparent)
(struct lambda-exp (argument body) #:transparent)
(struct app-exp (procedure argument) #:transparent)

(struct closure (argument body environment) #:transparent)


(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens 
  (= LET EQ ENDEQ IN LPAR RPAR COMMA SUB EOF ZERO IF 
     MINUS ADD MUL QUOTIENT EQUAL GREATER LESS PROC))

;; Lexer for the LET Language, Chapter 3
(define let-lexer
  (lexer
   [#\- 'SUB]
   [#\+ 'ADD]
   [#\* 'MUL]
   [#\/ 'QUOTIENT]
   [#\- 'SUB]
   ["minus" 'MINUS]
   [#\( 'LPAR]
   [#\) 'RPAR]
   [#\, 'COMMA]
   [#\= 'EQ]
   ["let" 'LET]
   ["in" 'IN]
   ["proc" 'PROC]
   ["zero?" 'ZERO]
   ["equal?" 'EQUAL]
   ["greater?" 'GREATER]
   ["less?" 'LESS]
   ["if" 'IF]
   [#\: 'ENDEQ]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) 
    ; =>
    (token-ID (string->symbol lexeme))]
   [(:+ (:or (char-range #\0 #\9) #\.)) 
    ; =>
    (token-NUM (string->number lexeme))]
   [#\% (let-comment-lexer input-port)]
   [(union #\space #\newline) (let-lexer input-port)]
   [(eof) 'EOF]))

;;lex comment secitons % 
(define let-comment-lexer
  (lexer
   [#\newline (let-lexer input-port)]
   [any-char (let-comment-lexer input-port)]
   [(eof) 'EOF]))



;;let token parser
(define let-parser
  (parser
   
   (start start)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   
   (precs 
    (left SUB))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(NUM) (const-exp $1)]
         [(ID) (var-exp $1)]
         [(LET exp EQ exp ENDEQ IN exp) 
          (let-exp $2 $4 $7)]
         [(IF exp exp exp)
          (if-exp $2 $3 $4)]
         ;;PROCEDURES creation handler
         [(PROC LPAR exp RPAR exp) 
          (lambda-exp $3 $5)]
         ;;SUB MINUS ADD MUL QUOTIENT
         [(SUB LPAR exp COMMA exp RPAR)
          (diff-exp $3 $5)]
         [(ADD LPAR exp COMMA exp RPAR)
          (add-exp $3 $5)]    
         [(MUL LPAR exp COMMA exp RPAR)
          (mul-exp $3 $5)]
         [(QUOTIENT LPAR exp COMMA exp RPAR)
          (quotient-exp $3 $5)]         
         [(MINUS LPAR exp RPAR)
          (minus-exp $3)]
         [(ZERO LPAR exp RPAR)
          (zero?-exp $3)]
         [(EQUAL LPAR exp COMMA exp RPAR)
          (equal?-exp $3 $5)]
         [(GREATER LPAR exp COMMA exp RPAR)
          (greater?-exp $3 $5)]
         [(LESS LPAR exp COMMA exp RPAR)
          (less?-exp $3 $5)]
         [(exp LPAR exp RPAR)
          (app-exp $1 $3)]
         ))))


;;Environment
;; empty-env : -> hash?
(define (empty-env)
  (hash))

;; extend-env : hash? symbol? number? -> hash?
(define (extend-env env var value)
  (hash-set env var value))

;; apply-env : hash? symbol? -> any?
(define (apply-env env var)
  (hash-ref env var))

;; join-env : hash? hash? -> hash?
(define (join-env env extend-env)
  (for/fold ([tmp env]) ([(key value) (in-hash extend-env)]) 
    (hash-set tmp key value)))


;;get the AST
;; string? -> struct-exp
(define (ast source)
  (define source-code (open-input-string source))
  (let-parser (lambda () (let-lexer source-code))))

;; run the AST
;; string? -> any?
(define (run source)
  (interp (ast source) (empty-env)))




;; interp the ast with the environment
;; struct-exp ->  any?
(define (interp exp env)
  (match exp
    [(const-exp num) num]
    [(var-exp var) (apply-env env var)]
    ;;Arithmetic Operations
    [(diff-exp exp1 exp2) (- (interp exp1 env) (interp exp2 env))]
    [(add-exp exp1 exp2) (+ (interp exp1 env) (interp exp2 env))]
    [(mul-exp exp1 exp2) (* (interp exp1 env) (interp exp2 env))]
    [(quotient-exp exp1 exp2) (quotient (interp exp1 env) (interp exp2 env))]
    [(minus-exp exp) (* (interp exp env) -1)]
    ;;Boolean Ops
    [(zero?-exp exp) (= (interp exp env) 0)]
    [(equal?-exp exp1 exp2) (= (interp exp1 env) (interp exp2 env))]
    [(greater?-exp exp1 exp2) (> (interp exp1 env) (interp exp2 env))]
    [(less?-exp exp1 exp2) (< (interp exp1 env)(interp exp2 env))]
    ;; Branching
    [(if-exp cond-exp if-exp else-exp)
     (define cond-result (interp cond-exp env))
     (if cond-result (interp if-exp env) (interp else-exp env))]
    ;; proc language extension
    ;; application/closure expression
    [(app-exp app-name app-body)
     (match-define ;get the closure from the environment
       (closure clos-argument clos-body clos-environment)
       (apply-env env (var-exp-var app-name)))
     ; this is the application exp, we need the result to feed the closure
     (define app-body-value (interp app-body env))
     ;the closure env is the current env plus the clos inner env
     (define new-closure-env
       (join-env 
        ;the close argument gets replaced by the application result
        (extend-env env (var-exp-var clos-argument) app-body-value) 
        clos-environment))
     (interp clos-body new-closure-env)]
    ;; lambda expression
    ;; a lambda expression evaluates to a closure
    [(lambda-exp body argument)
     (closure body argument env)]
    [(let-exp var-exp binding-exp body-exp)
     (define variable-name (var-exp-var var-exp))
     (define variable-value (interp binding-exp env))
     (interp body-exp (extend-env env variable-name variable-value))]))




#;
(run "
let z = 5:
in let x = 3:
in let y = -(x,1): % y = 2
in let x = 4: % x = 4
in -(z, -(x,y)) % 5 - (4 - 2)
")

#;(run "
let z = -(2 , 1): %complex expression
in let x = 3:
in -(z, -(x,1)) % here x = 4
")

#;(run "
let z = 0:
 in zero?(z)
")

#;(run "
let z = 0:
in if zero?(z) 1 2 
")

#;(run "
let z = 5: % z = 5
in let x = minus(3): % x = -3
in let y = +(x,1): % y = -2
in let x = 4: % x = 4
in *(z, -(x,y))  % 5 * -(4 - 2)
")

#;
(run "
let z = 2:
 in if less?(z, 1) 1 0 
")

#;(run "let f = proc(x) -(x, 11): in f(12)")
     
(run "
let x = 200: 
 in let f = proc (z) -(z,x): % f = z - 200
  in let x = 100: % x = 100
   in let g = proc (z) -(z,x):  %g = z - 100
    in -(f(1), g(1)) % (1 - 200) - (1 - 100)")


(module+ test
  (require rackunit)
  (check-pred lambda-exp? (ast "proc(x) -(x,11)"))
  (check-pred let-exp?  (ast "let f = proc(x) -(x, 11): in f(f(77))"))
  (check-equal? (run "let z = 2: in if less?(z,1) 1 0") 0)
  (check-equal? (run "let f = proc(x) -(x, 11): in f(12)") 1)
  (check-equal? (run "let z = 2: in if less?(z,1) 1 0") 0)
  (check-equal? -100 
                (run "
                      let x = 200: 
                       in let f = proc (z) -(z,x): % f = z - 200
                        in let x = 100: % x = 100
                         in let g = proc (z) -(z,x):  %g = z - 100
                          in -(f(1), g(1)) % (1 - 200) - (1 - 100)")))
